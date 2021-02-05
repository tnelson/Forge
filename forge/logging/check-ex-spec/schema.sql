/* SQL schema, hosted on Google Cloud
 * project = pyret-examples
 *
 * SQL ID = cs1710-2021-examplar
 * password = drracket
 * connection name = pyret-examples:us-east1:cs1710-2021-examplar
 * public IP = 34.75.228.82
 *
 * To connect using the `gcloud` script:
 *  $ ./google-cloud-sdk/bin/gcloud sql connect cs1710-2021-examplar -d cs1710_2021_examplar -u root
 * then enter the password.
 */

/* CREATE DATABASE cs1710_2021_examplar; */

CREATE TABLE checkexspec (
 id serial PRIMARY KEY,
 ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
 ip CHAR(40),
 local_id INT UNSIGNED,
 student VARCHAR(60),
 project VARCHAR(60),
 input MEDIUMTEXT,
 output MEDIUMTEXT
);
