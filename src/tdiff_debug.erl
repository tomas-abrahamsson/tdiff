-module(tdiff_debug).

-export([svg_diff/3]).

%% @doc Example:
%% ```
%%   svg_diff("A cat ate my hat", "A dog ate my shoe", "tdiff-trace.svg").
%% '''
%% The resulting svg file works well in Chrome and Opera.
%%
%% The Sx is (usually) the "old" string.
%% The Sy is (usually) the "new" string.
%% The blue number in the upper left is the D number.
%% Taking a step to the right means deleting that character.
%% Taking a step down means inserting that character.
%%
%% The tdiff algorithm can diff any list of objects, such as lists of
%% lines, lists of words or whatever.  However, this tracer is only
%% good at strings (lists of characters).  It might produce funny or
%% strange svg files in other cases.
%%
svg_diff(Sx, Sy, DestFileName) ->
    Tracer = start_svg_tracer(Sx, Sy, DestFileName),
    Res = tdiff:diff(Sx, Sy, [{algorithm_tracer, fun(Ev) -> Tracer ! Ev end}]),
    stop_tracer(Tracer),
    Res.

start_svg_tracer(Sx, Sy, DestFileName) ->
    proc_lib:spawn(fun() -> init_svg_tracer(Sx, Sy, DestFileName) end).

stop_tracer(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {stop, self()},
    receive
        {Pid, Res} ->
            erlang:demonitor(MRef, [flush]),
            Res;
        {'DOWN', MRef, _, _, Reason} ->
            erlang:error({terminated,Reason})
    end.

init_svg_tracer(Sx, Sy, DestFileName) ->
    FdInfo = open_svg_file(Sx, Sy, DestFileName),
    loop_svg_tracer(FdInfo, _D=unknown, Sx, Sy).

loop_svg_tracer(FdInfo, D, Sx, Sy) ->
    receive
        {final_edit_script, EditScript} ->
            print_final_edit_script(FdInfo, EditScript),
            loop_svg_tracer(FdInfo, D, Sx, Sy);
        {exhausted_kdiagonals, _D} ->
            print_svg_d_closed(FdInfo),
            loop_svg_tracer(FdInfo, D, Sx, Sy);
        {d, NewD} ->
            print_svg_d(FdInfo, NewD),
            loop_svg_tracer(FdInfo, NewD, Sx, Sy);
        {dpath, DPath} ->
            print_svg_dpath(FdInfo, DPath),
            loop_svg_tracer(FdInfo, D, Sx, Sy);
        {stop, From} ->
            print_svg_dmax(FdInfo, D),
            close_svg_file(FdInfo),
            From ! {self(),ok}
    end.

-record(fdinfo, {fd, xoffs, yoffs, dx, dy,
                 pathlinestyle,       endpointstyle,
                 final_pathlinestyle, final_endpointstyle}).

open_svg_file(Sx, Sy, DestFileName) ->
    SxLen = length(Sx),
    SyLen = length(Sy),
    DMax = SxLen + SyLen,
    Dx = 10,
    Dy = 10,
    XOffs = 20,
    YOffs = 10,
    {ok,Fd} = file:open(DestFileName, [write]),
    io:format(
      Fd,
      "<?xml version=\"1.0\" encoding=\"iso-8859-1\" standalone=\"no\"?>\n"
      "<svg xmlns:svg=\"http://www.w3.org/2000/svg\"\n"
      "     xmlns=\"http://www.w3.org/2000/svg\"\n"
      "     version=\"1.0\"\n"
      "     viewBox=\"0 0 ~w ~w\"\n"
      "     id=\"svg2\">\n"
      "  <style type=\"text/css\"><![CDATA[\n"
      "    .border { fill:none; stroke: #000000; stroke-width: 2; }\n"
      "    .grid   { fill:none; stroke: #cccccc; stroke-width: 0.33; }\n"
      "    .path   { fill:none; stroke: #660000; stroke-width: 1; }\n"
      "    .endpt  { fill:#660000; stroke: none; }\n"
      "    .fpath  { fill:none; stroke: #006600; stroke-width: 1; }\n"
      "    .fendpt { fill:#006600; stroke: none; }\n"
      "  ]]></style>\n"
      "  <script><![CDATA[\n"
      "function lightup(id) {\n"
      "    var t = document.getElementById(id)\n"
      "    t.setAttributeNS(null,\"fill-opacity\", 1);\n"
      "}\n"
      "function lightdown(id) {\n"
      "    var t = document.getElementById(id)\n"
      "    t.setAttributeNS(null,\"fill-opacity\", 0.75);\n"
      "}\n"
      "var d_min=0;\n"
      "var d_max=~w; // upper bound\n"
      "var curr_d=0;\n"
      "\n"
      "function set_d_visibility(d) {\n"
      "  for (i = d_min; i <= d_max; i++) {\n"
      "    var elem = document.getElementById(\"d\"+i.toString());\n"
      "    if (i == d-2) {\n"
      "      elem.setAttributeNS(null,\"fill-opacity\", 0.5);\n"
      "      elem.setAttributeNS(null,\"stroke-opacity\", 0.5);\n"
      "      elem.setAttributeNS(null,\"visibility\", \"visible\");\n"
      "    } else if (i == d-1) {\n"
      "      elem.setAttributeNS(null,\"fill-opacity\", 0.75);\n"
      "      elem.setAttributeNS(null,\"stroke-opacity\", 0.75);\n"
      "      elem.setAttributeNS(null,\"visibility\", \"visible\");\n"
      "    } else if (i == d) {\n"
      "      elem.setAttributeNS(null,\"visibility\", \"visible\");\n"
      "    } else {\n"
      "      elem.setAttributeNS(null,\"fill-opacity\", 1);\n"
      "      elem.setAttributeNS(null,\"stroke-opacity\", 1);\n"
      "      elem.setAttributeNS(null,\"visibility\", \"hidden\");\n"
      "    }\n"
      "    var txtelem = document.getElementById(\"d\"+i.toString()+\"text\");\n"
      "    if (i == d && txtelem)\n"
      "      txtelem.setAttributeNS(null,\"visibility\", \"visible\");\n"
      "    else\n"
      "      txtelem.setAttributeNS(null,\"visibility\", \"hidden\");\n"
      "  }\n"
      "}\n"
      "\n"
      "function do_prev() {\n"
      "  if (curr_d > d_min)\n"
      "    curr_d--;\n"
      "  set_d_visibility(curr_d);\n"
      "}\n"
      "function do_next() {\n"
      "  if (curr_d < d_max)\n"
      "    curr_d++;\n"
      "  set_d_visibility(curr_d);\n"
      "}\n"
      "  ]]></script>\n"
      ,[SxLen*Dx+2*XOffs+10, SyLen*Dy+2*YOffs, DMax]),

    io:format(Fd,
              "  <g id=\"nav\">\n", []),
    io:format(Fd,
              "     <path d=\"M ~w,~w v ~w l ~w,~w z\"\n"
              "           fill=\"#112233\" stroke=\"none\"\n"
              "           fill-opacity=\"0.75\"\n"
              "           id=\"prev\"\n"
              "           onmouseover=\"lightup('prev');\"\n"
              "           onmouseout=\"lightdown('prev');\"\n"
              "           onmousedown=\"do_prev();\"\n"
              "      />\n"
              , [round(Dx/4),0,
                 Dy,
                 -round(Dx/2), -round(Dy/2)]),
    io:format(Fd,
              "     <path d=\"M ~w,~w v ~w l ~w,~w z\"\n"
              "           fill=\"#112233\" stroke=\"none\"\n"
              "           fill-opacity=\"0.75\"\n"
              "           id=\"next\"\n"
              "           onmouseover=\"lightup('next')\"\n"
              "           onmouseout=\"lightdown('next')\"\n"
              "           onmousedown=\"do_next()\"\n"
              "      />\n"
              , [SxLen*Dx+2*XOffs-Dx,0,
                 Dy,
                 round(Dx/2), -round(Dy/2)]),
    io:format(Fd,
              "  </g>\n", []),

    BoxLineStyle       = o([{class,border}]),
    GridLineStyle      = o([{class,grid}]),
    PathLineStyle      = o([{class,path}]),
    EndPtStyle         = o([{class,endpt}]),
    FinalPathLineStyle = o([{class,fpath}]),
    FinalEndPtStyle    = o([{class,fendpt}]),

    io:format(Fd, "  <g id=\"gridlayer1\">\n", []),

    %% Grid lines...
    lists:foreach(fun(X) -> io:format(Fd,
                                      "    <path d=\"M ~w,~w v ~w\"\n ~s />\n"
                                      ,[X*Dx+XOffs, YOffs, SyLen*Dy,
                                        GridLineStyle])
                  end,
                  lists:seq(1,SxLen)),

    lists:foreach(fun(Y) -> io:format(Fd,
                                      "    <path d=\"M ~w,~w h ~w\"\n ~s />\n"
                                      ,[XOffs, Y*Dy+YOffs, SxLen*Dx,
                                        GridLineStyle])
                  end,
                  lists:seq(1,SyLen)),

    %% Box around
    io:format(
      Fd,
      "    <path d=\"M ~w,~w v ~w h ~w v -~w z\" ~s />\n"
      ,[XOffs, YOffs, SyLen*Dy, SxLen*Dx, SyLen*Dy, BoxLineStyle]),

    %% Symbols for each X/Y
    lists:foreach(
      fun({X,C}) ->
              io:format(
                Fd,
                "    <text x=\"~w\" y=\"~.2f\"\n"
                "          font-size=\"8\" text-anchor=\"middle\">~c</text>\n",
                [XOffs+Dx*X+round(Dx/2), YOffs-YOffs/4, C])
      end,
      index0seq(Sx)),
    lists:foreach(
      fun({Y,C}) ->
              io:format(
                Fd,
                "    <text x=\"~w\" y=\"~.2f\"\n"
                "          font-size=\"8\" text-anchor=\"middle\">~c</text>\n",
                [XOffs-Dx/2, YOffs+Dy*(Y+1)-YOffs/4, C])
      end,
      index0seq(Sy)),

    io:format(Fd, "  </g>\n", []),


    #fdinfo{fd                  = Fd,
            xoffs               = XOffs,
            yoffs               = YOffs,
            dx                  = Dx,
            dy                  = Dy,
            pathlinestyle       = PathLineStyle,
            endpointstyle       = EndPtStyle,
            final_pathlinestyle = FinalPathLineStyle,
            final_endpointstyle = FinalEndPtStyle}.

index0seq([]) -> [];
index0seq(L)  -> lists:zip(lists:seq(0,length(L)-1), L).

print_svg_d(#fdinfo{fd=Fd, xoffs=XOffs,yoffs=YOffs, dx=Dx}, D) ->
    io:format(Fd,
              "  <g id=\"d~wtext\" visibility=\"~s\">\n"
              "    <text x=\"~w\" y=\"~.2f\" font-size=\"8\"\n"
              "          text-anchor=\"end\"\n"
              "          fill=\"#0000cc\">~w</text>\n"
              "  </g>\n"
              ,[D, if D == 0 -> "visible"; true -> "hidden" end,
                XOffs-Dx*0.25, YOffs-YOffs/4, D]),
    io:format(Fd,
              "  <g id=\"d~w\" visibility=\"~s\">\n"
              ,[D, if D == 0 -> "visible"; true -> "hidden" end]).

print_svg_d_closed(#fdinfo{fd=Fd}) ->
    io:format(Fd, "  </g>\n", []).


print_svg_dpath(FdInfo, {_X, _Y, _Sx, _Sy, RSteps}=_DPath) ->
    #fdinfo{pathlinestyle = PathLineStyle,
            endpointstyle = EndPtStyle} = FdInfo,
    print_svg_dpath_2(FdInfo, RSteps, PathLineStyle, EndPtStyle).

print_svg_dpath_2(FdInfo, RSteps, PathLineStyle, EndPtStyle) ->
    #fdinfo{fd                  = Fd,
            xoffs               = XOffs,
            yoffs               = YOffs,
            dx                  = Dx,
            dy                  = Dy} = FdInfo,

    io:format(Fd, "    <path d=\"M ~w,~w ", [XOffs, YOffs]),
    {X,Y} = lists:foldl(
              fun({Dir, _C}, {X0,Y0}) ->
                      %% X,Y seems to be reversed below [1]
                      %% but they are not. This is because of
                      %% how x,y are defined (removing a char
                      %% from Sx inserting a char from Sy,
                      %% respectively).
                      {X1,Y1} = if Dir == e -> {1,1};
                                   Dir == x -> {0,1}; %% [1] here...
                                   Dir == y -> {1,0}  %% ... and here
                                end,
                      io:format(Fd, " l ~w,~w", [X1*Dx, Y1*Dx]),
                      {X0+X1, Y0+Y1}
              end,
              {0,0},
              lists:reverse(RSteps)),
    io:format(Fd, "\"\n          ~s\n    />\n", [PathLineStyle]),
    io:format(Fd,
              "    <circle cx=\"~w\" cy=\"~w\" r=\"~.2f\"\n"
              "            ~s />\n"
              ,[XOffs+X*Dx,YOffs+Y*Dy, Dx/4, EndPtStyle]).

print_final_edit_script(FdInfo, EditScript) ->
    #fdinfo{final_pathlinestyle = PathLineStyle,
            final_endpointstyle = EndPtStyle} = FdInfo,
    RSteps1 = lists:append(
                lists:map(fun({ins,Elems}) -> [{x,E} || E <- Elems];
                             ({del,Elems}) -> [{y,E} || E <- Elems];
                             ({eq,Elems})  -> [{e,E} || E <- Elems]
                          end,
                          EditScript)),
    RSteps2 = lists:reverse(RSteps1),
    print_svg_dpath_2(FdInfo, RSteps2, PathLineStyle, EndPtStyle).

print_svg_dmax(#fdinfo{fd=Fd}, D) ->
    io:format(
      Fd,
      "  <script><![CDATA[\n"
      "var d_max=~w;\n"
      "  ]]></script>\n"
      ,[D]).

close_svg_file(#fdinfo{fd=Fd}) ->
    %% Trailer
    io:format(
      Fd,
      "  </g>\n"
      "</svg>\n"
      ,[]),
    file:close(Fd),
    ok.

o(L) -> case o2(L) of
            " "++Rest -> Rest;
            Os        -> Os
        end.

o2(L) ->
    lists:flatten(
      lists:map(
        fun({K,V}) when is_atom(V)    -> [" ",a2s(K), "=\"", a2s(V), "\""];
           ({K,V}) when is_integer(V) -> [" ",a2s(K), "=\"", i2s(V), "\""];
           ({K,V}) when is_list(V)    -> [" ",a2s(K), "=\"", V,      "\""]
        end,
        L)).

a2s(A) -> atom_to_list(A).
i2s(I) -> integer_to_list(I).
