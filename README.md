#### What is it

A (not very efficient) Erlang implementation of the
_O(ND) differnence algorithm_ by Eugene W. Myers
see link: https://www.google.com/search?q=eugene+myers+difference+algorithm+filetype:pdf

#### Example
<pre><code>
1> tdiff:diff("A cat ate my hat", "A dog ate my shoe").
[{eq,"A "},
 {del,"cat"},
 {ins,"dog"},
 {eq," ate my "},
 {ins,"s"},
 {eq,"h"},
 {del,"at"},
 {ins,"oe"}]
</code></pre>

There is also a debugging engine that generates a series of svg files,
for visualizing the progress of the diff algorithm:

<pre><code>
2> tdiff_debug:svg_diff("A cat ate my hat", "A dog ate my shoe",
                        "/tmp/tdiff-trace.svg").

unixprompt% firefox /tmp/diff-trace.svg
</code></pre>

The resulting svg works with at least Firefox and Chromium/Google Chrome.

#### On the todo

The algorithm currently searches only from the beginning to the
end. It does not search in both directions, so it ends up
searching very many diagonals, most often in vain, so there is room for
memory and performance improvements.

Currently, the algorithm only always takes the tail of the list, so it
could be possible to use a lazy (memoizing) list.

#### References

Much good info about diff, match and patch can found at
link: http://neil.fraser.name/writing/diff/
