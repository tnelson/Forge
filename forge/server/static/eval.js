function MakeAjaxRequest(data){
    req = new XMLHttpRequest();
    req.onreadystatechange = function() {
        if (req.readyState == 4 && req.status == 200) {
            document.getElementById("eval-output").innerHTML += this.responseText + "\n";
        }
      };
    req.open('POST', evalurl, true);
    req.setRequestHeader('Content-Type','text/xml');
    req.setRequestHeader('expr',data);
    req.send(data);
    console.log(evalurl);
}

function SendQuery(query){
    if(event.key === 'Enter') {
        MakeAjaxRequest(query.value);        
    }
}
