---
type: page
layout: default
title: Shang-Lin Chen - Official website
logo: Shang-Lin Chen
---

**W**elcome to Shang-Lin Chen's home on the web! 

I [blog]({{site.baseurl}}/blog/) here about programming, Linux, and other technical topics. I also write [code]({{site.baseurl}}/projects/) and other [stuff]({{site.baseurl}}/writing/).
 
I keep a list of dance resources and LA-area dances on my [dance page]({{site.baseurl}}/dance/).

[Want to know more?]({{site.baseurl}}/about/)

<h3>Latest Blog Posts</h3>
<!-- <ul class="post-list"> -->
<table>
{% for post in site.posts limit:3 %}
<!-- <li><span class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</span>&nbsp;
<a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a>&nbsp;</li> -->
<tr><td class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</td><td><a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a></td></tr>
{% endfor %}
<!-- </ul> -->
</table>
<br>

