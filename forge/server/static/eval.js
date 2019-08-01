function MakeAjaxRequest(data){
    req = new XMLHttpRequest();
    req.onreadystatechange = function() {
        if (req.readyState == 4 && req.status == 200) {
            var eout =  document.getElementById("eval-output");
            eout.innerHTML += this.responseText + "<br>";
            eout.scrollTop = eout.scrollHeight;
            document.getElementById("eval-input").innerHTML = "";
        }
      };
    req.open('POST', evalurl, true);
    req.setRequestHeader('Content-Type','text/xml');
    req.setRequestHeader('expr',data);
    req.send(data);
    console.log(evalurl);
}

function SendQuery(query){
    if (event.key === 'Enter') {
        MakeAjaxRequest(query.value);
        document.getElementById("eval-output").innerHTML += "\> " + query.value + "<br>";
    }
}
