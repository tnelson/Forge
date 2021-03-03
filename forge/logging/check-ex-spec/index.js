const PROJECT_ID = "pyret-examples";
const SQL_ID = "cs1710-2021-examplar";
const DATABASE = "cs1710_2021_examplar";
const TABLE_ID = "checkexspec";

const mysql = require("mysql");

function getConnection () {
  const options = 
  {
    host: "34.75.228.82",
    socketPath: "/cloudsql/" + PROJECT_ID + ":us-east1:" + SQL_ID,
    user: 'root',
    password: 'drracket',
    database: DATABASE
  };
  return mysql.createConnection(options);
}

exports.recv = (req, res) => {
  res.set('Access-Control-Allow-Origin', '*');
  if (req.method === 'POST') {
    let conn = getConnection();
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
    const ip = req.headers['x-forwarded-for'] || req.connection.remoteAddress;
    const post = {
      ip: ip,
      ts: data.ts,
      local_id: data.local_id,
      student: data.student,
      project: data.project,
      input: (data.input.charCodeAt(0) < 130 && data.input[0] !== '/') ? data.input : "",
      output: data.output
    };
    conn.query("INSERT INTO " + TABLE_ID + " SET ?", post, function(err, result) {
      if (err) {
        console.error(err);
        return res.status(500).send({error: 'unable to post', err});
      } else {
        return res.status(200).send({});
      }
    });
  }
};

