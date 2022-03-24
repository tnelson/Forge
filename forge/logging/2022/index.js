const PROJECT_ID = "pyret-examples";
const SQL_ID = "lfs2022x";
const DATABASE = "lfs2022x";
const TABLE_ID = "main";
const TABLE_ADDR = "34.75.227.215";

const mysql = require("mysql");

const Firestore = require('@google-cloud/firestore');
const COLLECTION_NAME = 'events';

function getSQLConnection () {
  const options = 
  {
    host: TABLE_ADDR,
    socketPath: "/cloudsql/" + PROJECT_ID + ":us-east1:" + SQL_ID,
    user: 'root',
    password: 'drracket',
    database: DATABASE
  };
  return mysql.createConnection(options);
}

function getFirestore () {
  const options =
  {
    projectId: PROJECT_ID,
    timestampsInSnapshots: true
  };
  return new Firestore(options);
}

exports.submit = (req, res) => {
  //console.log("lfs2022x begin");
  //console.log(req.body);
  res.set('Access-Control-Allow-Origin', '*');
  if (req.method === 'OPTIONS') {
    // Send response to OPTIONS requests
    res.set('Access-Control-Allow-Methods', 'POST');
    res.set('Access-Control-Allow-Headers', 'Content-Type');
    res.set('Access-Control-Max-Age', '3600');
    res.status(204).send('');
  } else if (req.method === 'POST') {
    let data;
    if (typeof(req.body) === 'string' || req.body instanceof String) {
      try {
        data = JSON.parse(req.body);
      } catch (e) {
        data = req.body;
      }
    } else if (req.body instanceof Object) {
      data = req.body;
    } else {
      data = req.body;
    }
    //console.log("lfs2022x data ok")
    const ip = req.headers['x-forwarded-for'] || req.connection.remoteAddress;
    const timestamp = new Date().getTime();
    //console.log("lfs2022x conn ok");
    let conn;
    let post;
    if (data.payload) {
      // Examplar data, send to firestore
      conn = getFirestore();
      post = {
        ip: ip,
        timestamp: timestamp,
        app_timestamp: data.time,
        session: data.session,
        event: data.event,
        email: data.email,
        assignment: data.assignment,
        payload: data.payload,
      };
      return conn.collection(COLLECTION_NAME)
        .add(post)
        .then(doc => {
          return res.status(200).send({});
        }).catch(err => {
          console.error(err);
          return res.status(500).send({ error: 'unable to store', err });
        });
    } else if (data.ts) {
      conn = getSQLConnection();
      // Forge data, send to SQL
      post = {
        ip: ip,
        ts: data.ts,
        local_id: data.local_id,
        student: data.student,
        lang: data.lang,
        project: data.project,
        input: (data.input && data.input.charCodeAt(0) < 130 && data.input.charCodeAt(1) < 130 && data.input.charCodeAt(2) < 130 && data.input.charCodeAt(3) < 130) ? data.input : "",
        output: data.output
      };
      //console.log("lfs2022x posting")
      conn.query("INSERT INTO " + TABLE_ID + " SET ?", post, function(err, result) {
        if (err) {
          console.error(err);
          conn.end();
          return res.status(500).send({error: 'unable to post', err});
        } else {
          conn.end();
          return res.status(200).send({});
        }
      });
    } else {
      conn.end();
      return res.status(203).send({});
    }
  }
};

