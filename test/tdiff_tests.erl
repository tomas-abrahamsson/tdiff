%%% Copyright (C) 2011  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with this library; if not, write to the Free
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-module(tdiff_tests).
-include_lib("eunit/include/eunit.hrl").


simple_diff_test() ->
    [{eq,"a"},{del,"B"},{ins,"X"},{eq,"ccc"},{del,"D"},{ins,"Y"},{eq,"e"}] =
        tdiff:diff("aBcccDe", "aXcccYe").

completely_mismatching_test() ->
    [{del,"aaa"}, {ins,"bbb"}] = tdiff:diff("aaa", "bbb").

empty_inputs_produces_empty_diff_test() ->
    [] = tdiff:diff("", "").

only_additions_test() ->
    [{ins,"aaa"}] = tdiff:diff("", "aaa"),
    [{eq,"a"},{ins,"b"},{eq,"a"},{ins,"b"},{eq,"a"},{ins,"b"},{eq,"a"}] =
        tdiff:diff("aaaa", "abababa").

only_deletions_test() ->
    [{del,"aaa"}] = tdiff:diff("aaa", ""),
    [{eq,"a"},{del,"b"},{eq,"a"},{del,"b"},{eq,"a"},{del,"b"},{eq,"a"}] =
        tdiff:diff("abababa", "aaaa").

patch_test() ->
    Diff = tdiff:diff(Old="a cat ate my hat", New="a dog ate my shoe"),
    New = tdiff:patch(Old, Diff).

diff_patch_binaries_test() ->
    [{del,["The Naming of Cats is a difficult matter,\n"]},
     {ins,["The Naming of Dogs is a different matter,\n"]},
     {eq,["It isn't just one of your holiday games;\n",
          "You may think at first I'm as mad as a hatter\n"]},
     {del,["When I tell you, a cat must have THREE DIFFERENT NAMES.\n"]}] =
        Diff =
        tdiff:diff_binaries(
          %% T.S. Elliot:
          Old =
              <<"The Naming of Cats is a difficult matter,\n"
                "It isn't just one of your holiday games;\n"
                "You may think at first I'm as mad as a hatter\n"
                "When I tell you, a cat must have THREE DIFFERENT NAMES.\n">>,
          %% Not T.S. Elliot (of course):
          New =
              <<"The Naming of Dogs is a different matter,\n"
                "It isn't just one of your holiday games;\n"
                "You may think at first I'm as mad as a hatter\n">>),
    New = list_to_binary(tdiff:patch_binaries(Old, Diff)).
