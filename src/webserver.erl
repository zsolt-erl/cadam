-module(webserver).

-export([start/1, stop/0]).


start(Port)->
    os:putenv("YAWSHOME","/tmp/yaws/"),
    case whereis(yaws_sup) of
	undefined ->
	    Res=yaws:start_embedded("priv/www", 
				    [{port,Port},{listen,{0,0,0,0}},
				     {partial_post_size,102400}],                %% sconf, 100k post size
				    [{include_dir, ["priv/www/"]}]),             %% gconf
	    case Res of
		ok -> {ok, Port};
		_Other -> error
	    end;
	_Else ->
	    ok
    end.
	     
    

stop()->
    yaws:stop().
