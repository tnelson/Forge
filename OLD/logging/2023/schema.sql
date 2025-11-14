/* SQL schema, hosted on Google Cloud
 * project = pyret-examples
 *
 * SQL ID = lfs2023
 * password = drracket
 * connection name = pyret-examples:us-east1:lfs2023
 * public IP = 35.231.81.85
 *
 * To connect using the `gcloud` script:
 *  $ ./google-cloud-sdk/bin/gcloud sql connect lfs2023 -d lfs2023 -u root
 * then enter the password
 * then enter the table name "main"
 */

/* CREATE DATABASE lfs2023; */

CREATE TABLE main (
 id serial PRIMARY KEY,
 ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
 ip CHAR(40),
 local_id INT UNSIGNED,
 student VARCHAR(60),
 forgeversion CHAR(40),
 lang VARCHAR(60),
 project VARCHAR(60),
 input MEDIUMTEXT,
 output MEDIUMTEXT
);

