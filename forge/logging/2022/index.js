const PROJECT_ID = "pyret-examples";
const SQL_ID = "lfs2022x";
const DATABASE = "lfs2022x";
const TABLE_ID = "main";
const TABLE_ADDR = "34.75.227.215";

const mysql = require("mysql");

function getConnection () {
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

exports.submit = (req, res) => {
  //console.log("lfs2022x begin");
  //console.log(req.body);
  res.set('Access-Control-Allow-Origin', '*');
  if (req.method === 'POST') {
    let conn = getConnection();
    //console.log("lfs2022x conn ok");
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
    let post;
    if (data.payload) {
      post = null;
    } else {
      post = {
      ip: ip,
      ts: data.ts,
      local_id: data.local_id,
      student: data.student,
      lang: data.lang,
      project: data.project,
      input: (data.input && data.input.charCodeAt(0) < 130 && data.input.charCodeAt(1) < 130 && data.input.charCodeAt(2) < 130 && data.input.charCodeAt(3) < 130) ? data.input : "",
      output: data.output
    }};
    //console.log("lfs2022x posting")
    if (post) {
    conn.query("INSERT INTO " + TABLE_ID + " SET ?", post, function(err, result) {
      if (err) {
        console.error(err);
        return res.status(500).send({error: 'unable to post', err});
      } else {
        return res.status(200).send({});
      }
    });
    }
  }
};


