-define(TRANSCHILD(I, Type, Args), {I, {I, start_link, Args}, transient, 5000, Type, [I]}).
-define(TEMPCHILD(I, Type, Args),  {I, {I, start_link, Args}, temporary, 5000, Type, [I]}).
-define(PERMCHILD(I, Type, Args),  {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(PGV(Key, List), proplists:get_value(Key, List)).

