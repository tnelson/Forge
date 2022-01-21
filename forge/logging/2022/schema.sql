/* SQL schema, hosted on Google Cloud
 * project = pyret-examples
 *
 * SQL ID = lfs2022
 * password = drracket
 * connection name = pyret-examples:us-east1:lfs2022
 * public IP = 34.138.172.34
 *
 * To connect using the `gcloud` script:
 *  $ ./google-cloud-sdk/bin/gcloud sql connect lfs2022 -d lfs2022 -u root
 * then enter the password
 * then enter the table name "main"
 */

/* CREATE DATABASE lfs2022; */

CREATE TABLE main (
 id serial PRIMARY KEY,
 ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
 ip CHAR(40),
 local_id INTEGER,
 student VARCHAR(60),
 lang VARCHAR(60),
 project VARCHAR(60),
 input TEXT,
 output TEXT
);

