/* ************************************************************************

   Copyright:

   License:

   Authors: Zsolt Keszthelyi  <zsolt.erl@gmail.com>

************************************************************************ */

/* ************************************************************************

#asset(cadamgui/*)

************************************************************************ */

qx.Class.define("cadamgui.Application",
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
	  // decorator.setBackgroundImage("cadam/bl1280.jpg");
	  // root.set({decorator:decorator});
	
	  var mainContainer = new qx.ui.container.Composite( new qx.ui.layout.VBox(10) );
	  var headerContainer = new qx.ui.container.Composite( new qx.ui.layout.VBox(10) );
	  var workContainer   = new qx.ui.container.Composite( new qx.ui.layout.HBox() );

	  root.add(mainContainer);
	  mainContainer.add(headerContainer);
	  mainContainer.add(workContainer);
	  
	  var menubar   = new qx.ui.toolbar.ToolBar();
	  menubar.setWidth(2500);
	  menubar.setFont( new qx.bom.Font(16, ["Verdana", "Courier"]).set({bold: true}) );

	  menubar.add( new qx.ui.basic.Label("CADAM v0.1").set({padding: 7}) );
	  menubar.add( new qx.ui.toolbar.Separator() );
	  var toolsMenu = new qx.ui.toolbar.MenuButton("Tools");
	  menubar.add( toolsMenu );
	  menubar.add( new qx.ui.toolbar.Separator() );
	  var usageLabel = new qx.ui.basic.Label().set(
	      {
		  value: "Drag and Drop a metric from the list onto one of the empty gray containers!",
		  marginLeft: 20,
		  alignY: "middle"
	      });
	  menubar.add( usageLabel );

	  headerContainer.add(menubar);
	  
	  
	  var metricsList = new qx.ui.form.List().set(
	  {
	      textColor: "black",
	      font: new qx.bom.Font(12, ["Verdana", "Courier"]),
	      margin: 20,
	      width: 350,
	      height: 530,
	      draggable: true
	  });
	  workContainer.add(metricsList);

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
	
	  // create grid
	  var dashboardGrid = new qx.ui.container.Composite( new qx.ui.layout.Grid(10,10) );
	  dashboardGrid.set(
	  {
	      marginTop: 20
	  });
	  workContainer.add( dashboardGrid );
	
	  // populate grid with empty containers
	  var widgets = this.widgets = [];
	  for (var i=0; i<2; i++) {
	      widgets[i] = [];
	      for (var j=0; j<3; j++) {
		  widgets[i][j] = new qx.ui.container.Composite( new qx.ui.layout.VBox() ).set(
		  {
		      backgroundColor: "#3a3a3a",
		      width: 400,
		      height: 170,
		      droppable: true
		  });
		  widgets[i][j].addListener("drop", this.showChart, this);
		  dashboardGrid.add(widgets[i][j], {column: i, row: j});
	      };
	  };

	  
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
      showChart : function(e)
      {
	  var ContainerWidget = e.getTarget();
	  var metricID = e.getData("metricid");

	  var Header = new qx.ui.container.Composite( new qx.ui.layout.HBox(0, "right"));
	  ContainerWidget.add(Header);
	  
	  // add label
	  var Label = new qx.ui.basic.Label( metricID[0]+' - '+metricID[1]).set(
	      {
		  textColor: "yellow",
		  font: new qx.bom.Font(12, ["Verdana", "Courier"]),
		  marginTop: 2,
		  marginRight: 20
	      });
	  Header.add(Label);

	  var CloseButton = new qx.ui.basic.Image("cadamgui/close-button.png");
	  CloseButton.addListener("click", function(e){
				      this.ws.sendJSON({cmd: "unsubscribe_from_queue", metricid: metricID, queuenum: 1});
				      // take timeline out of metric_routes array
				      delete this.metric_routes[metricID];
				      ContainerWidget.removeAll();
				  }, this);


	  Header.add(CloseButton);

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
	  ContainerWidget.add(chart);	
	  var ts = chart.getTimeseries();
	  this.metric_routes[metricID] = ts;
	  this.ws.sendJSON({cmd: "subscribe_to_queue", metricid: metricID, queuenum: 1});
      }
  }
});
