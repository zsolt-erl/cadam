%% event tracer detail levels for the different debug levels
-define(ERROR_DETAIL_LEVEL,       10).
-define(WARNING_DETAIL_LEVEL,     30).
-define(INFO_DETAIL_LEVEL,        50).
-define(DEBUG_DETAIL_LEVEL,       60).
-define(APP_METRICS_DETAIL_LEVEL, 80).
-define(SYS_METRICS_DETAIL_LEVEL, 90).



%% macros for reporting application level metrics
-define(APP_METRICS(Name, Value), et:report_event(?APP_METRICS_DETAIL_LEVEL, node(), metrics, Name, [{Name, Value}])).
-define(APP_METRICS_LIST(Label, Proplist), et:report_event(?APP_METRICS_DETAIL_LEVEL, node(), metrics, Label, [Proplist])).

%% macros for reporting system level metrics
-define(SYS_METRICS(Name, Value), et:report_event(?SYS_METRICS_DETAIL_LEVEL, node(), metrics, Name, [{Name, Value}])).
-define(SYS_METRICS_LIST(Label, Proplist), et:report_event(?SYS_METRICS_DETAIL_LEVEL, node(), metrics, Label, [Proplist])).

%% NOTE: lists are not handled by the event_handler yet so don't use APP_METRICS_LIST and SYS_METRICS_LIST

%% -define(CDINFO(Name, Value), et:report_event(?APP_METRICS_DETAIL_LEVEL, node(), metrics, Name, [{Name, Value}]).


