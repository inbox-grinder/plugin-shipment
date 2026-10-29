//! Conservative labelled carrier mail and host-approved USPS tracking reports.
use chrono::{DateTime, NaiveDate, SecondsFormat};
use plugin_sdk::{
    EmailInput, FieldCandidate, RecordContribution, RecordIdentity, RecordResult, RefreshInput,
    RefreshRequest, SemanticUpdate, SourceRef,
};
use serde_json::{json, Value};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
plugin_sdk::export_plugin!(extract);
plugin_sdk::export_refresh!(refresh_report);

fn empty(sources: Vec<SourceRef>) -> RecordResult {
    RecordResult {
        schema_version: "1".into(),
        sources,
        records: vec![],
        refresh_requests: vec![],
    }
}
fn timestamp(s: &str) -> Option<String> {
    DateTime::parse_from_rfc3339(s)
        .ok()
        .or_else(|| DateTime::parse_from_rfc2822(s).ok())
        .map(|d| d.to_utc().to_rfc3339_opts(SecondsFormat::Secs, true))
}
fn day(s: &str) -> Option<String> {
    (s.len() == 10)
        .then(|| NaiveDate::parse_from_str(s, "%Y-%m-%d").ok())
        .flatten()
        .map(|d| d.to_string())
}
fn expected(s: &str) -> Option<String> {
    timestamp(s).or_else(|| day(s).map(|d| format!("{d}T00:00:00Z")))
}
fn hash(value: &Value) -> String {
    format!("{:x}", Sha256::digest(value.to_string().as_bytes()))
}
fn carrier(s: &str) -> Option<&'static str> {
    match s.to_ascii_lowercase().as_str() {
        "usps" | "united states postal service" => Some("usps"),
        "ups" | "united parcel service" => Some("ups"),
        "fedex" | "federal express" => Some("fedex"),
        "dhl" => Some("dhl"),
        _ => None,
    }
}
fn number(s: &str) -> Option<String> {
    let s = s.trim().to_ascii_uppercase();
    ((8..=40).contains(&s.len()) && s.bytes().all(|c| c.is_ascii_alphanumeric())).then_some(s)
}
fn status(s: &str) -> Option<&'static str> {
    match s.to_ascii_lowercase().as_str() {
        "label created" | "pre-shipment" => Some("label_created"),
        "shipped" | "accepted" | "in transit" | "in_transit" => Some("in_transit"),
        "out for delivery" | "out_for_delivery" => Some("out_for_delivery"),
        "delayed" => Some("delayed"),
        "delivered" => Some("delivered"),
        "cancelled" | "canceled" => Some("cancelled"),
        _ => None,
    }
}
fn identity(carrier: &str, tracking: &str, shipped: &str) -> RecordIdentity {
    RecordIdentity {
        namespace: format!("shipment:{carrier}"),
        key: hash(&json!([tracking, shipped])),
    }
}
fn url(carrier: &str, tracking: &str) -> String {
    match carrier {
        "usps" => format!("https://tools.usps.com/go/TrackConfirmAction?tLabels={tracking}"),
        "ups" => format!("https://www.ups.com/track?tracknum={tracking}"),
        "fedex" => format!("https://www.fedex.com/fedextrack/?trknbr={tracking}"),
        _ => format!("https://www.dhl.com/global-en/home/tracking.html?tracking-id={tracking}"),
    }
}
fn path(tracking: &str) -> String {
    format!("/tracking/v3/tracking/{tracking}?expand=DETAIL")
}
fn label<'a>(lines: &'a [String], name: &str) -> Option<&'a str> {
    let values: BTreeSet<_> = lines
        .iter()
        .filter_map(|line| {
            line.split_once(':')
                .filter(|(key, _)| key.eq_ignore_ascii_case(name))
                .map(|(_, v)| v.trim())
        })
        .collect();
    (values.len() == 1)
        .then(|| *values.first().unwrap())
        .filter(|s| !s.is_empty())
}

pub fn extract(input: EmailInput) -> RecordResult {
    let mut result = empty(
        input
            .invocation
            .as_ref()
            .map(|i| i.sources.clone())
            .unwrap_or_default(),
    );
    let Some(source) = input.message.source_id.as_ref().filter(|id| {
        result
            .sources
            .iter()
            .any(|s| &s.id == *id && s.kind == "email")
    }) else {
        return result;
    };
    let Some(observed) = input.date.as_deref().and_then(timestamp) else {
        return result;
    };
    let owned = input
        .invocation
        .as_ref()
        .map(|i| i.records.as_slice())
        .unwrap_or(&[]);
    let text = input
        .text_body
        .filter(|s| !s.trim().is_empty())
        .unwrap_or_else(|| html_text(input.html_body.as_deref().unwrap_or("")));
    // A carrier label begins each parcel. Paragraphs within a parcel are harmless.
    let mut blocks: Vec<Vec<String>> = vec![];
    for line in text.lines().map(str::trim).filter(|s| !s.is_empty()) {
        if line
            .split_once(':')
            .is_some_and(|(k, _)| k.eq_ignore_ascii_case("carrier"))
        {
            blocks.push(vec![]);
        }
        if let Some(block) = blocks.last_mut() {
            block.push(line.to_owned());
        }
    }
    let mut records = BTreeMap::new();
    let mut ambiguous = BTreeSet::new();
    for lines in blocks {
        let Some(carrier) = label(&lines, "Carrier").and_then(carrier) else {
            continue;
        };
        let Some(tracking) = label(&lines, "Tracking number").and_then(number) else {
            continue;
        };
        let Some(status) = label(&lines, "Status").and_then(status) else {
            continue;
        };
        // A generation is required rather than guessing from delivery time or a thread.
        let Some(shipped) = label(&lines, "Shipment date").and_then(day) else {
            continue;
        };
        let mut values = base(carrier, &tracking, &shipped, status);
        for (label_name, field) in [
            ("Expected delivery", "expectedAt"),
            ("Origin", "origin"),
            ("Destination", "destination"),
            ("Location", "location"),
        ] {
            if let Some(value) = label(&lines, label_name) {
                if field == "expectedAt" {
                    if let Some(date) = expected(value) {
                        values.insert(field.into(), json!(date));
                    }
                } else {
                    values.insert(field.into(), json!(value));
                }
            }
        }
        let occurred = label(&lines, "Updated at")
            .and_then(timestamp)
            .unwrap_or_else(|| observed.clone());
        if occurred
            .get(..10)
            .is_none_or(|date| date < shipped.as_str())
        {
            continue;
        }
        let record = contribution(values, source, &occurred, owned);
        if records
            .insert(record.identity.clone(), record.clone())
            .is_some()
        {
            ambiguous.insert(record.identity);
        }
    }
    for (id, record) in records {
        if ambiguous.contains(&id) {
            continue;
        }
        if record.fields["carrier"].value == "usps" {
            result.refresh_requests.push(RefreshRequest {
                identity: id,
                service: "usps".into(),
                path: path(record.fields["trackingNumber"].value.as_str().unwrap()),
                interval_seconds: 3600,
            });
        }
        result.records.push(record);
    }
    // Host limits each atomic result to sixteen service requests.
    result.refresh_requests.truncate(16);
    result
}
fn base(carrier: &str, tracking: &str, shipped: &str, status: &str) -> BTreeMap<String, Value> {
    BTreeMap::from([
        ("carrier".into(), json!(carrier)),
        ("trackingNumber".into(), json!(tracking)),
        ("shipmentDate".into(), json!(shipped)),
        ("status".into(), json!(status)),
        ("url".into(), json!(url(carrier, tracking))),
    ])
}
fn contribution(
    values: BTreeMap<String, Value>,
    source: &str,
    observed: &str,
    owned: &[Value],
) -> RecordContribution {
    let c = values["carrier"].as_str().unwrap();
    let tracking = values["trackingNumber"].as_str().unwrap();
    let status = values["status"].as_str().unwrap();
    let id = identity(c, tracking, values["shipmentDate"].as_str().unwrap());
    let completed = ["delivered", "cancelled"].contains(&status);
    // Terminal events have one meaning even if email and carrier provide different optional detail.
    let update_fields: Vec<String> = if completed {
        vec!["status".into()]
    } else {
        values.keys().cloned().collect()
    };
    let update_key = if completed {
        status.to_owned()
    } else {
        format!("tracking:{}", hash(&json!(values)))
    };
    let occurred = owned
        .iter()
        .find(|r| r["identity"]["namespace"] == id.namespace && r["identity"]["key"] == id.key)
        .and_then(|r| r["updates"][&update_key]["occurredAt"].as_str())
        .unwrap_or(observed)
        .to_owned();
    let summary = format!("Shipment {status}");
    let title = format!("{} · {tracking}", c.to_ascii_uppercase());
    RecordContribution {
        identity: id,
        record_type: "shipment".into(),
        fields: values
            .into_iter()
            .map(|(name, value)| {
                (
                    name.clone(),
                    FieldCandidate {
                        value,
                        evidence: vec![source.into()],
                        basis: "extracted".into(),
                        confidence: 1.0,
                        observed_at: if completed && name != "status" {
                            observed.to_owned()
                        } else {
                            occurred.clone()
                        },
                        revision: None,
                    },
                )
            })
            .collect(),
        updates: vec![SemanticUpdate {
            key: update_key,
            occurred_at: occurred.clone(),
            summary,
            notify: true,
            evidence: vec![source.into()],
            fields: update_fields,
            supersedes: None,
        }],
        attention: vec![],
        links: vec![],
        presentation: json!({"family":"shipment","version":1,"title":title}),
        lifecycle: if completed {
            json!({"state":"completed","completedAt":occurred,"graceSeconds":86400})
        } else {
            json!({"state":"active"})
        },
    }
}

pub fn refresh_report(input: RefreshInput) -> RecordResult {
    let mut result = empty(input.invocation.sources.clone());
    if input.schema_version != "1"
        || input.invocation.kind != "refresh"
        || input.service.id != "usps"
        || !result
            .sources
            .iter()
            .any(|s| s.id == input.service.source_id && s.kind == "remote")
    {
        return result;
    }
    let response = &input.service.response;
    let Some(tracking) = response["trackingNumber"].as_str().and_then(number) else {
        return result;
    };
    if input.service.resource != format!("https://apis.usps.com{}", path(&tracking)) {
        return result;
    }
    let matching: Vec<_> = input
        .invocation
        .records
        .iter()
        .filter(|r| {
            r["fields"]["carrier"]["candidate"]["value"] == "usps"
                && r["fields"]["trackingNumber"]["candidate"]["value"] == tracking
        })
        .collect();
    if matching.len() != 1 {
        return result;
    }
    let Some(shipped) = matching[0]["fields"]["shipmentDate"]["candidate"]["value"]
        .as_str()
        .and_then(day)
    else {
        return result;
    };
    let Some(status) = response["statusCategory"].as_str().and_then(status) else {
        return result;
    };
    // The latest scan supplies domain time. Failed/undated responses make no changes.
    let Some((occurred, event)) = response["trackingEvents"].as_array().and_then(|events| {
        events
            .iter()
            .filter_map(|e| {
                e["eventTimestamp"]
                    .as_str()
                    .and_then(timestamp)
                    .map(|t| (t, e))
            })
            .max_by(|a, b| a.0.cmp(&b.0))
    }) else {
        return result;
    };
    // A recycled tracking number must not import a scan from its prior shipment.
    if occurred
        .get(..10)
        .is_none_or(|date| date < shipped.as_str())
    {
        return result;
    }
    let mut values = base("usps", &tracking, &shipped, status);
    if let Some(eta) = response["expectedDeliveryDate"].as_str().and_then(expected) {
        values.insert("expectedAt".into(), json!(eta));
    }
    for (field, names, obj) in [
        (
            "origin",
            vec!["originCity", "originState", "originZIP"],
            response,
        ),
        (
            "destination",
            vec!["destinationCity", "destinationState", "destinationZIP"],
            response,
        ),
        (
            "location",
            vec!["eventCity", "eventState", "eventZIP"],
            event,
        ),
    ] {
        let location = names
            .iter()
            .filter_map(|n| obj[*n].as_str())
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join(", ");
        if !location.is_empty() {
            values.insert(field.into(), json!(location));
        }
    }
    let record = contribution(
        values,
        &input.service.source_id,
        &occurred,
        &input.invocation.records,
    );
    if matching[0]["identity"]["namespace"] != record.identity.namespace
        || matching[0]["identity"]["key"] != record.identity.key
    {
        return result;
    }
    result.records.push(record);
    result
}

fn html_text(html: &str) -> String {
    let mut out = String::new();
    let mut tag = String::new();
    let mut inside = false;
    let mut hidden = false;
    for c in html.chars() {
        match c {
            '<' => {
                inside = true;
                tag.clear();
            }
            '>' if inside => {
                inside = false;
                let name = tag
                    .split_whitespace()
                    .next()
                    .unwrap_or("")
                    .to_ascii_lowercase();
                if ["script", "style"].contains(&name.as_str()) {
                    hidden = true
                }
                if ["/script", "/style"].contains(&name.as_str()) {
                    hidden = false
                }
                if !hidden
                    && ["br", "br/", "/p", "/div", "/tr", "/li", "/h1", "/h2"]
                        .contains(&name.as_str())
                {
                    out.push('\n')
                }
                if !hidden && ["/td", "/th"].contains(&name.as_str()) {
                    out.push(' ')
                }
            }
            _ if inside => tag.push(c),
            _ if !hidden => out.push(c),
            _ => {}
        }
    }
    html_escape::decode_html_entities(&out).into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    fn input() -> EmailInput {
        serde_json::from_str(include_str!("../fixtures/usps.json")).unwrap()
    }
    fn snapshot(r: &RecordContribution) -> Value {
        json!({"identity":r.identity,"fields":r.fields.iter().map(|(k,v)| (k.clone(),json!({"candidate":v}))).collect::<BTreeMap<_,_>>(),
            "updates":r.updates.iter().map(|u| (u.key.clone(),json!({"occurredAt":u.occurred_at}))).collect::<BTreeMap<_,_>>()})
    }
    fn remote(record: Value) -> RefreshInput {
        serde_json::from_value(json!({"schemaVersion":"1","invocation":{"schemaVersion":"1","kind":"refresh","capabilities":["records","remote-service","scheduled-refresh"],"config":{},"records":[record],"sources":[{"id":"remote","kind":"remote"}]},"service":{"id":"usps","resource":format!("https://apis.usps.com{}",path("9400000000000000000000")),"sourceId":"remote","response":serde_json::from_str::<Value>(include_str!("../fixtures/usps-delivered-response.json")).unwrap()}})).unwrap()
    }
    #[test]
    fn carrier_fixtures_multiple_shipments_and_reused_numbers() {
        let fixtures = [
            include_str!("../fixtures/usps.json"),
            include_str!("../fixtures/ups.json"),
            include_str!("../fixtures/fedex.json"),
            include_str!("../fixtures/dhl.json"),
        ];
        let mut multiple = input();
        multiple.text_body = Some(
            fixtures
                .iter()
                .map(|s| {
                    serde_json::from_str::<Value>(s).unwrap()["textBody"]
                        .as_str()
                        .unwrap()
                        .to_owned()
                })
                .collect::<Vec<_>>()
                .join("\n\n"),
        );
        let result = extract(multiple);
        assert_eq!(result.records.len(), 4);
        assert_eq!(result.refresh_requests.len(), 1);
        assert!(result
            .records
            .iter()
            .all(|r| r.fields.values().all(|f| f.evidence == ["source-1"])));
        let mut reused = input();
        reused.date = Some("2027-10-25T12:00:00Z".into());
        reused.text_body = reused
            .text_body
            .map(|s| s.replace("Shipment date: 2026-10-24", "Shipment date: 2027-10-24"));
        assert_ne!(
            extract(input()).records[0].identity,
            extract(reused).records[0].identity
        );
    }
    #[test]
    fn unrelated_mail_and_html_keep_identity_and_event_meaning() {
        let first = extract(input());
        let mut duplicate = input();
        duplicate.date = Some("2026-10-26T12:00:00Z".into());
        duplicate.message.message_id = "<other-thread>".into();
        duplicate.invocation.as_mut().unwrap().records = vec![snapshot(&first.records[0])];
        duplicate.html_body = duplicate
            .text_body
            .take()
            .map(|s| format!("<div>{}</div>", s.replace('\n', "</div><div>")));
        let result = extract(duplicate);
        assert_eq!(
            serde_json::to_value(&first.records).unwrap(),
            serde_json::to_value(&result.records).unwrap()
        );
        assert_eq!(
            first.records[0].fields["expectedAt"].value,
            "2026-10-27T22:00:00Z"
        );
        let mut changed = input();
        changed.text_body = changed
            .text_body
            .map(|s| s.replace("2026-10-27T18:00:00-04:00", "2026-10-28T18:00:00-04:00"));
        assert_ne!(
            first.records[0].updates[0].key,
            extract(changed).records[0].updates[0].key
        );
    }
    #[test]
    fn delivery_refresh_and_later_mail_share_one_semantic_event() {
        let first = extract(input());
        let remote = refresh_report(remote(snapshot(&first.records[0])));
        assert_eq!(remote.records.len(), 1);
        assert_eq!(remote.records[0].updates[0].key, "delivered");
        assert_eq!(remote.records[0].lifecycle["state"], "completed");
        assert!(remote.records[0].attention.is_empty());
        let mut mail: EmailInput =
            serde_json::from_str(include_str!("../fixtures/delivered.json")).unwrap();
        mail.invocation.as_mut().unwrap().records = vec![snapshot(&remote.records[0])];
        let email = extract(mail);
        let a = &remote.records[0].updates[0];
        let b = &email.records[0].updates[0];
        assert_eq!(
            (&a.key, &a.summary, &a.occurred_at, &a.fields),
            (&b.key, &b.summary, &b.occurred_at, &b.fields)
        );
        assert_eq!(remote.records[0].identity, email.records[0].identity);
        assert_eq!(
            email.records[0].lifecycle["completedAt"],
            "2026-10-27T16:00:00Z"
        );
    }
    #[test]
    fn rejects_missing_evidence_ambiguous_identity_and_wrong_remote_resource() {
        for token in [
            "Carrier: USPS",
            "Shipment date: 2026-10-24",
            "Tracking number: 9400000000000000000000",
            "Status: in transit",
        ] {
            let mut mail = input();
            mail.text_body = mail.text_body.map(|s| s.replace(token, ""));
            assert!(extract(mail).records.is_empty());
        }
        let mut mail = input();
        mail.message.source_id = Some("forged".into());
        assert!(extract(mail).records.is_empty());
        let mut mail = input();
        mail.text_body = mail.text_body.map(|s| format!("{s}\n{s}"));
        assert!(extract(mail).records.is_empty());
        let r = snapshot(&extract(input()).records[0]);
        let mut wrong = remote(r.clone());
        wrong.service.resource.push_str("evil");
        assert!(refresh_report(wrong).records.is_empty());
        let mut wrong = remote(r.clone());
        wrong.service.response["trackingNumber"] = json!("9999999999");
        assert!(refresh_report(wrong).records.is_empty());
        let mut stale = remote(r.clone());
        stale.service.response["trackingEvents"][0]["eventTimestamp"] =
            json!("2025-10-27T16:00:00Z");
        assert!(refresh_report(stale).records.is_empty());
        let mut wrong = remote(r);
        wrong.service.response["trackingEvents"] = json!([]);
        assert!(refresh_report(wrong).records.is_empty());
    }
}
