/* SQL schema, hosted on Google Cloud
 * project = pyret-examples
 *
 * SQL ID = lfs2022x
 * password = drracket
 * connection name = pyret-examples:us-east1:lfs2022x
 * public IP = 34.75.227.215
 *
 * To connect using the `gcloud` script:
 *  $ ./google-cloud-sdk/bin/gcloud sql connect lfs2022x -d lfs2022x -u root
 * then enter the password
 * then enter the table name "main"
 */

/* CREATE DATABASE lfs2022x; */

CREATE TABLE main (
 id serial PRIMARY KEY,
 ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
 ip CHAR(40),
 local_id INT UNSIGNED,
 student VARCHAR(60),
 lang VARCHAR(60),
 project VARCHAR(60),
 input MEDIUMTEXT,
 output MEDIUMTEXT
);

