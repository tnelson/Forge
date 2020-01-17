window.onload = function () {
    // var sock = new WebSocket("ws://localhost:8081/test", "subprotocol");
    var sock = new WebSocket("ws://localhost:8081/test");
    var messagecounter = 0;
    sock.onopen = function() {
	console.log('open', arguments);
	sock.send("Hi there");
    };
    sock.onmessage = function(e) {
	console.log('message', e.data);
	messagecounter++;
	if (messagecounter == 10) {
	    sock.send("goodbye");
	}
    };
    sock.onclose = function() {
	console.log('close', arguments);
    };
};
