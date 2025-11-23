import json
import csv

# Sample input JSON string from HTML5-QRCode
html5_qr_output = '{"type":"URL","url":"https://www.canto.io","params":{"wallet":"wallet123","network":"bostrom"}}'

# Parse JSON
data = json.loads(html5_qr_output)

# Convert to CSV
csv_file = 'qr_output.csv'
with open(csv_file, 'w', newline='') as file:
    writer = csv.writer(file)
    # Write header
    writer.writerow(['type', 'url', 'wallet', 'network'])
    # Write data row
    writer.writerow([data['type'], data['url'], data['params']['wallet'], data['params']['network']])

# Convert to JSON-LD
json_ld = {
    "@context": "https://schema.org",
    "@type": "WebPage",
    "url": data['url'],
    "identifier": data['params']['wallet'],
    "additionalProperty": {
        "@type": "PropertyValue",
        "name": "network",
        "value": data['params']['network']
    }
}

# Save JSON-LD
with open('qr_output.jsonld', 'w') as f:
    json.dump(json_ld, f, indent=2)
