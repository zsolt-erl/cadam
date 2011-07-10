/* ************************************************************************

   Copyright:

   License:

   Authors: Zsolt Keszthelyi  <zsolt.erl@gmail.com>

************************************************************************ */

/* ************************************************************************

#asset(cadamgui/*)

************************************************************************ */

qx.Class.define("cadamgui.Application_withWindows",
{
  extend : qx.application.Standalone,



  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

  members :
  {
      timeseries :    [],
      metric_routes : [],
      ws :            {},

      gridTop :      120,
      gridLeft :     400,
      gridSpaceX :   20,
      gridSpaceY :   20,
      windowWidth :  404,
      windowHeight : 154,


      
      /**
     * This method contains the initial application code and gets called 
     * during startup of the application
     * 
     * @lint ignoreDeprecated(alert)
     */
      main : function()
      {
	  // Call super class
	  this.base(arguments);

	  // Enable logging in debug variant
	  if (qx.core.Variant.isSet("qx.debug", "on"))
	  {
              // support native logging capabilities, e.g. Firebug for Firefox
              qx.log.appender.Native;
              // support additional cross-browser console. Press F7 to toggle visibility
              qx.log.appender.Console;
	  }

	  var root = this.getRoot();
	  var timeseries = this.timeseries;
	  var metric_routes = this.metric_routes;

	  /*
	   * ---------------------------------------------------------------------------
	   * CREATE UI
	   * ---------------------------------------------------------------------------
	   */
	
	  // set up background
	  // var decorator=new qx.ui.decoration.Single();
	  // decorator.setBackgroundImage("mtagui/bl1280.jpg");
	  // root.set({decorator:decorator});
	
	  var mainContainer = new qx.ui.container.Composite( new qx.ui.layout.VBox(10) );
	  var headerContainer = new qx.ui.container.Composite( new qx.ui.layout.VBox(10) );
	  var workContainer   = new qx.ui.container.Composite( new qx.ui.layout.Canvas() );

	  root.add(mainContainer);
	  mainContainer.add(headerContainer);
	  mainContainer.add(workContainer);
	  
	  var menubar   = new qx.ui.toolbar.ToolBar();
	  menubar.setWidth(2500);
	  var toolsMenu = new qx.ui.toolbar.MenuButton("Tools");

	  menubar.add( new qx.ui.basic.Label("CADAM v0.1").set({padding: 7}) );
	  menubar.add( new qx.ui.toolbar.Separator() );
	  // menubar.add( toolsMenu );
	  headerContainer.add(menubar);
	  
	  var usageLabel = new qx.ui.basic.Label( "Drag and Drop a metric from the list onto the gray area!" );
	  usageLabel.set(
	  {
//	      font: new qx.bom.Font(18, ["Verdana", "Courier"]),
	      textColor: "white",
	      marginLeft: 50
	  });
	  menubar.add(usageLabel);
	  
	  var metricsList = new qx.ui.form.List().set(
	  {
	      textColor: "black",
	      width: 300,
	      maxHeight: 400,
	      draggable: true
	  });
	  workContainer.add(metricsList, {left: 50, top: this.gridTop});

	  metricsList.addListener("dragstart", function(e)
	  {
	      e.addAction("move");
	      e.addType("metricid");
	  });
	  
	  metricsList.addListener("droprequest", function(e)
	  {
	      var type = e.getCurrentType();
	      var action = e.getCurrentAction();
	      if ((type=="metricid") && (action=="move")) {
		  var metricid = this.getSelection()[0].getModel();
		  e.addData(type, metricid);
	      }
	  });
	  
	  var desktop = this.desktop = new qx.ui.window.Desktop( new qx.ui.window.Manager() ).set(
	      {
		  width: (this.windowWidth + 20) * 2 + this.gridSpaceX,
		  height: this.windowHeight * 3 + this.gridSpaceY * 2,
		  backgroundColor: "#3a3a3a",
		  droppable: true
	      });
	      
	  workContainer.add( desktop, {left: this.gridLeft, top: this.gridTop});
	  desktop.addListener("drop", this.showChartW, this);

	      
	  /*
	   * ---------------------------------------------------------------------------
	   * COMMUNICATE WITH SERVER
	   * ---------------------------------------------------------------------------
	   */

	  // create a function to handle messages from server
          var wsonmessage = function(m) {
	      if (m.data){
		  var message = qx.util.Json.parse(m.data);
		  console.log(message);
		  switch (message.response_for) {
		      case 'get_all_metrics' :
			  var metrics = message.response;
			  var listItem;
			  metricsList.removeAll();
			  for (var i=0; i<metrics.length; i++) {
			      listItem = new qx.ui.form.ListItem(metrics[i][0]+" - "+metrics[i][1], null, metrics[i]);
			      metricsList.add(listItem);
			  }
			  break;
		      case 'subscription' :
			  var metricid = message.response.metricid;
			  var value = message.response.value;
			  var ts = metric_routes[ metricid ];
			  // append new value to timeseries
			  ts.append(new Date().getTime(), value);
			  break;
		      default :
			  this.parent.debug("unexpected response:"+message.response_for);
		  }
              }
        };
	
	// create websocket object
	var ws = this.ws = new cadamgui.WebSocket2();
	// overwrite onmessage call of the websocket
	ws.onmessage = wsonmessage;
	ws.connect("ws://localhost:9000/metrics_ws_server_connector.yaws");
	ws.addListener("socket-ready", function()
	{
	    ws.sendJSON({cmd: "get_all_metrics"});			   
	});
      },
      


      // e is the drop event, 'this' is the qx.Application
      showChartW : function(e)
      {
	  var windowRealHeight = this.windowHeight + 20;  // adjust for title bar

	  var ContainerWidget = e.getTarget();
	  var metricID = e.getData("metricid");

	  var cursorX = e.getDocumentLeft();
	  var cursorY = e.getDocumentTop();
	  
	  Window = new qx.ui.window.Window( metricID[0]+' - '+metricID[1] ).set(
	      {
		  width:          this.windowWidth,
		  height:         this.windowHeight,
		  contentPadding: 2,
		  
		  movable:      true,
		  resizable:    false,
		  showMaximize: false,
		  showMinimize: false
	      });
	  Window.setLayout(new qx.ui.layout.VBox());
	  var windowX = Math.floor((cursorX - this.gridLeft) / (this.windowWidth + this.gridSpaceX)) * 
	      (this.windowWidth + this.gridSpaceX);// + this.gridLeft;

	  var windowY = Math.floor((cursorY - this.gridTop) / (windowRealHeight + this.gridSpaceY)) * 
	      (windowRealHeight + this.gridSpaceY);// + this.gridTop;

	  this.desktop.add(Window, {left: windowX, top: windowY});
	  
	  // create/add smoothie chart
          var chartOptions = {grid: {verticalSections : 2, millisPerLine : 10000}, maxValueScale: 1.1, 
			      fps : 20, millisPerPixel : 100};
	  var chart = new cadamgui.QxSmoothie(chartOptions, 4000).set(
	      {
		  canvasWidth: 400,
		  maxWidth: 400,
		  canvasHeight: 150,
		  height: 150
	      });
	  Window.add(chart);	
	  Window.open();
	  var ts = chart.getTimeseries();
	  this.metric_routes[metricID] = ts;
	  this.ws.sendJSON({cmd: "subscribe_to_queue", metricid: metricID, queuenum: 1});
	  console.log('drop listener - metric_routes='+qx.util.Json.stringify(this.metric_routes));
      }



  }
});
