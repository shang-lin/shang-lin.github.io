---
type: page
layout: default
title: Shang-Lin Chen - Official website
logo: Shang-Lin Chen
---

**W**elcome to my home on the web! 

My name is Shang-Lin Chen, and I am a programmer / Linux admin / dabbler in many subjects. I received my BS degree in computer science from [Caltech](http://caltech.edu). On paper, my degree is in Engineering and Applied Sciences because Caltech did not offer a computer science major for undergraduates until a year after I graduated. I now enjoy taking [MOOCs](https://en.wikipedia.org/wiki/Massive_open_online_course) [in](https://www.coursera.org) [various](https://www.udacity.com) [subjects](https://edx.org).

More information about my past and present technical projects is available on [my projects page]({{site.baseurl}}/projects/).

I also practice [tai chi chuan](http://taijitips.com) and [dance]({{site.baseurl}}/dance/). For free tai chi classes in Pasadena, CA, try the [Caltech Tai Chi Club](http://www.its.caltech.edu/~dotaichi/).

<h3>Latest Blog Posts</h3>
<ul class="post-list">
{% for post in site.posts limit:3 %}
<li><span class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</span>&nbsp;
<a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a>&nbsp;</li>
{% endfor %}
</ul>
