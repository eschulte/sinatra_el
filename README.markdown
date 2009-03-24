Emacs support for using the [Sinatra](http://www.sinatrarb.com/) web development DSL

This includes...

* a major mode combining ruby-mode and haml-mode for your main sinatra
  file
* a command <tt>sinatra-console</tt> <tt>\C-;c</tt> for quickly
  running irb in your sinatra environment
* a command <tt>sinatra-web-server</tt> <tt>\C-;w</tt> for quickly
  running your sinatra web-server in an Emacs comint buffer allowing
  jumping from errors to source
* bundled with some basic ruby modes

To use sinatra_el...

1. add this directory to your load path
2. add this to your .emacs init file <tt>(require 'sinatra)</tt>
3. open your sinatra application file, and run <tt>M-x sinatra-mode</tt>

*Note*: due to limitation of MuMaMo-mode you must add the following
haml comment line

<pre>
-#end-of-file
</pre>

to the bottom of your file for haml mode to turn on following the
<tt>\_\_END\_\_</tt> marker
