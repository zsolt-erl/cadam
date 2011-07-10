qx.Class.define("cadamgui.WebSocket2",
{
    extend : qx.core.Object,

    events : 
    {
	"socket-ready" : "qx.event.type.Data"
    },
    
    members :
    {
	_wsock     : {},
	_connected : false,
	
        _onopen: function(){
	    console.log('websocket connecting');
            this.send('client-connected');
	    this.parent._connected = true;
	    this.parent.fireDataEvent("socket-ready", true);
	},

	_onclose: function(m) {
	    console.log('websocket closing');
            this._wsock = null;
	    this._connected = false;
        },

	sendTxt: function(msg){
	    if (this._connected) {
		console.log('sending:'+msg);
		this._wsock.send(msg);
	    }
	},

	sendJSON: function(msg){
	    if (this._connected) {
		var msgString = qx.util.Json.stringify(msg);
		console.log('sending:'+msgString);
		this._wsock.send(msgString);
	    }
	},
	
	// example endpoint: "ws://localhost:9000/metrics_ws_server_connector.yaws"
        connect: function(endpoint){
	    var ws = this._wsock = new WebSocket(endpoint);
	    ws.onopen    = this._onopen;
            ws.onmessage = this.onmessage;
            ws.onclose   = this._onclose;
	    ws.parent    = this;
        },

        onmessage: function(m) {
          if (m.data){
              var text = m.data; 
	      console.log('got data:'+text);
          }
        }
    }
});