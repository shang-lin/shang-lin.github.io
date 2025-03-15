---
title: Reinstalling Jekyll on Big Sur
---

During the three years that I haven't updated this website, I finally updated my late-2013 Macbook Pro to Big Sur, the final Mac OS that it can support. Today, I realized that Jekyll, the Ruby-based static website framework I use to update this website locally, wasn't working. Here is what I did to get it working:

{% highlight shell %}
$ brew install ruby
$ echo 'export PATH="$HOME/.gem/ruby/3.4.0/bin:/usr/local/Cellar/ruby/3.4.2/bin:$PATH"' >> ~/.zshrc
$ gem install --user-install jekyll
{% endhighlight %}

This installed the latest stable version of Ruby (3.4.2 at the time), added Ruby and gem paths to my $PATH environment variable, and installed Jekyll in my user's local gem path.

I moved to the website's directory and ran:
{% highlight shell %}
$ bundle update
{% endhighlight %}

to update all gems.

Then I could start serving the website by running:
{% highlight shell %}
$ bundle exec jekyll build
$ bundle exec jekyll serve
{% endhighlight %}

