-author("falvik").

-record(node_info,{
  pid,
  monitor,
  links=[]
}).

-type(node_id()     :: term()).
-type(nodes_map()   :: #{node_id() => #node_info{}}).
