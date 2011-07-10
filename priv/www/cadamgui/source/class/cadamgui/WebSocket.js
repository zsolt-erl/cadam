qx.Class.define("cadamgui.WebSocket",
{
    extend : qx.core.Object,

    properties:
    {
	ws : {nullable : true}
    },
    
    members :
    {
//	ws : {},

        _onopen: function(){
	    this.parent.debug('websocket connecting');
            this.send('client-connected');
	},
        _onclose: function(m) {
	    this.parent.debug('websocket closing');
            //this._ws=null;
        },
	_send: function(msg) {
	    this.send(msg);
	},
	send: function(msg){
	    if (this.getWs()) this.getWs().send(msg);
	},

	// example endpoint: "ws://localhost:9000/metrics_ws_server_connector.yaws"
        connect: function(endpoint){
	    this.ws = new WebSocket(endpoint);
            this.ws.onopen    = this._onopen;
            this.ws.onmessage = this.onmessage;
            this.ws.onclose   = this._onclose;
	    this.ws.parent    = this;
        },
//        sendMsg: function(message){
//	    this._ws.send(message);
//            if (this._ws) this._ws.send(message);
//        },
        onmessage: function(m) {
          if (m.data){
              var text = m.data; 
	      this.parent.debug('got data:'+text);
          }
        }
    }
});