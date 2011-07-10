Overview
--------

CADAM stands for Cluster And Distributed Application Monitor. It's mainly a debugging/monitoring tool for distributed
applications but can be used for just monitoring a cluster.  
It uses the event\_collector module from the Event Tracker application and basically implements a general event\_viewer
behaviour that is used by other modules that process the collected events.   
The benefit of using the event\_collector is that reporting an event adds almost no overhead to a application when it is 
not being monitored (see [et:report_event/5][et]). This way you can leave debug or info reports in a production system 
without slowing it down and run CADAM to monitor it when needed.

Currently the only fully workable module is the metrics module (cadam\_metrics). This module collects metrics data from 
the nodes in a cluster (eg. memory usage, cpu load or any other metrics data that your application exposes) and shows it
real time on charts in a web interface using [SmoothieCharts][].  
There's also a logger module (cadam\_logger) but at this point all it does is it logs the events into a text file.

- [Installation](#install)
- [Running](#run)   

>- [From the Erlang shell](#shell)
- [From the command line](#cli)
- [Web GUI](#gui)
- [Queues](#queues)

- [Creating your own metrics](#metrics)
- [Monitoring a Riak Cluster](#riak)

<a name=install></a>
Installation
------------

#### Requirements

Needs Erlang R13B03 or higher. Needs pam and ssl development packages to build Yaws.
Since the backend sends data to the web GUI via a websocket you will need a WebSocket capable browser (eg. Chrome or 
Firefox 4.0).
You can also access the collected metrics data via an API in which case there's obviously no browser requirement.

#### Clone or Download and Unpack

    git clone git://github.com/zsolt-erl/cadam cadam

or

    after downloading 
    tar -xvzf cadam.tar.gz

#### Compile

    make


#### Edit conf/cadam.config
You only need to edit this if the cluster you are monitoring is using a different version of the Erlang VM then CADAM
 is running on or the Event Tracker application (which is part of a standard Erlang install) is not available on the 
cluster nodes. In this case see comments in the config file.

#### Edit conf/cadam_cli.conf
If you are running CADAM from the command line (not from the Erlang shell) then edit this to set up the node name and 
the cookie.



<a name=run></a>
Running
-------
<a name=shell></a>
### From the Erlang shell

    erl -name t -setcookie metis -pa ebin -boot start_sasl -config conf/cadam
    Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [hipe] [kernel-poll:false]  
    Eshell V5.8.4  (abort with ^G)

__Start CADAM app__  

    (t@example.net)1> cadam:start().

    =INFO REPORT==== 8-Jul-2011::19:21:02 ===  
    Yaws: Listening to 0.0.0.0:9000 for <1> virtual servers:  
     - http://localhost:9000 under priv/www  
    Started webserver on port 9000  
    Access web gui at: http://localhost:9000/gui  
    ok  

__Connect to a node on the cluster you want to monitor__  

    (t@example.net)2> cadam:connect(server@example.net).  
    ['server@example.net']  
    (t@example.net)3> nodes().  
    ['server@example.net','queue@example.net']  

__Start metrics collection__  

    (t@example.net)4> cadam:start_module(cadam_metrics).  
    Started metrics generators on all connected nodes  
    {ok,<0.91.0>}  

__List of metrics that are being collected__  

    (t@example.net)5> cadam_metrics:get_all_metrics().
    [{'queue@example.net',memory_atom},
     {'queue@example.net',memory_total},
     {'queue@example.net',process_count},
     {'server@example.net',memory_atom},
     {'server@example.net',memory_total},
     {'server@example.net',process_count},
     {'queue@example.net',queue_length}]


__Queues of collected values for a metric  (explained later)__

    (t@example.net)6> cadam_metrics:get_queues({'queue@example.net',queue_length}).  
    {[35812,35813,35814,35815,35816,35817,35818,35819,35820,
      35821,35822,35823,35824,35825,35826,35827,35828,35829,35830,
      35831,35832,35833,35834,35835,35836,35837,35838,35839|...],
     [35938,36049,36161,36288,36414,36525,36637,36764,36892,
      37004,37117,37245,37373,37484,37596,37715,37835,37961,38087,
      38199,38313,38443,38570,39107],
     []}

__Stop metrics module__

    (t@example.net)7> cadam:stop_module(cadam_metrics).  

__Stop CADAM app__

    (t@example.net)8> cadam:stop().  
    ok  

<a name=cli></a>
### From the command line

    >./cadam_cli help
    Usage: cadam_cli {start|stop|ping|help|nodes|start-module|stop-module|viewer|connect}

    >./cadam_cli start

    >./cadam_cli connect server@example.net

    >./cadam_cli nodes
    Nodes connected to the CADAM node:
        server@example.net
        queue@example.net

    >./cadam_cli start-module cadam_metrics
    Starting module: cadam_metrics ... {ok,<6595.98.0>}

<a name=gui></a>
### Web GUI

The Web GUI can be accessed at:

    http://localhost:9000/gui/

This can be used when the metrics module is running.  
The GUI is pretty simple. It show the list of metrics on the left side and there are chart containers on the right side.
Drag a metric from the list and drop it onto an empty container. This will subscribe to a feed of the metric's collected 
values and show it in a chart updated real time.

<a name=queues></a>
### Queues

Metric values are collected and stored in linked queues. A queue collects values for a certain time interval then averages 
the collected values, stores this average and also sends it to processes that subscribed to this queue.  
The metric module currently has 3 queues for each collected metric with time intervals of 2 sec, 1 min, 5 min. The 2 sec 
queue sends its average to the 1 min queue at timeout and the 1 min queue sends its average to the 5 min queue. Therefore 
a process subscribing to the 1 min queue (queue number 2) will receive *{QueuePid, tsqueue, Average}* messages every 
1 minute.

    cadam_metrics:get_queues(MetricID).

can be used to see the list of collected values (head of the list is the most current one).

    cadam_metrics:subscribe_to_queue(MetricID, QueueNum, Pid).

will subscribe the Pid process to a queue.

    cadam_metrics:unsubscribe_from_queue(MetricID, QueueNum, Pid).

will unsubscribe the Pid process from a queue.


<a name=metrics></a>
Creating your own metrics
-------------------------

Include __cadam_macros.hrl__ located in the __cadam/include/__ directory in your module.

Use the __?APP_METRICS(Name, Value)__ macro to report a metric value to CADAM.

example:

    -module(measure_temperature).
    -export([start/0]).
    -include("cadam_macros.hrl").

    start()->
       loop().


    loop()->
       receive
       after
          1000->
             ?APP_METRICS(temperature, random:uniform(1000))
       end,
       loop().

<a name=riak></a>
Monitoring a Riak cluster
-------------------------

A Riak release does not include the Event Tracker application therefore the needed modules have to be loaded into the 
Riak Erlang VM. This is done by setting the *netload_et* option to *true* in *conf/cadam.config*.
This will work if you installed Riak from source and compiled CADAM with the same Erlang version.

If you are using R14 something to run CADAM and installed a prepackaged Riak (eg. from the Ubuntu repo) you might have to
do the following trick.   
In addition to the above the CADAM node and the Riak nodes need to use the same Event Tracker beam files. This means 
that if you are using R14B03 to run CADAM those beams won't load into an older VM (eg. Riak installed from an older Ubuntu 
repo. that was compiled with R13B03). To handle this you can take the beam files of an Erlang R13B03 installation 
(they are under *erlang/lib/et-x.y.z/ebin*, copy them to *cadam/priv/et_beams* and set *et_beam_path* to *"priv/et_beams"* 
in *conf/cadam.config*. The included beams were compiled on 64bit R13B03.
Ideally you would also compile *cadam/src/cadam_metrics_generators.erl* on the older Erlang VM and put it into the same 
dir. since this module also gets loaded in the Riak Erlang VM, however I did not have to do this when running CADAM on 
R14B03 and using Riak from the Ubuntu repo.


CADAM by default will monitor memory usage and process count on all nodes.
I'm sure there are some more interesting metrics that could be monitored on a Riak cluster I just did not get to figure 
that out yet. Also the Riak source could be modified to expose some metrics data.

[SmoothieCharts]: http://github.com/joewalnes/smoothie
[et]: http://www.erlang.org/doc/man/et.html