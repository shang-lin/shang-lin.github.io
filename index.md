---
type: page
layout: default
title: Shang-Lin Chen - Official website
logo: Shang-Lin Chen
---

**W**elcome to Shang-Lin Chen's home on the web! From here you can:

* Visit [Code Words]({{site.baseurl}}/blog/), my blog about programming and technology.
* Read more [about me]({{site.baseurl}}/about/).
* Check out my [projects]({{site.baseurl}}/projects/).
* See my list of [dance resources and LA-area dances]({{site.baseurl}}/dance/).

<h3>Latest Blog Posts</h3>
<ul class="post-list">
{% for post in site.posts limit:3 %}
<li><span class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</span>&nbsp;
<a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a>&nbsp;</li>
{% endfor %}
</ul>

<div>
<a href="https://www.linkedin.com/in/shanglinchen"><img src="images/In-2C-14px.png"></a>
</div>
