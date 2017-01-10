When you visit a web page, a malicious third party sniffing on your network could read everything you send. For this reason, websites that collect payment or identification information are supposed to use HTTPS, a protocol that encrypts web traffic. You might have noticed the lock icon and the "https://" prefix when you visit a website that uses HTTPS. 

HTTPS is now recommended even for websites that do not ask for your credit card. This year, Google will begin marking non-HTTPS sites as insecure, starting with sites that collect login or credit card information, but eventually expanding to all websites. What does this mean for you? If you run a website and don't This means that your personal or business website could take a hit in its search rankings if you don't implement HTTPS. 

HTTPS uses an encryption protocol called TLS (Transport Layer Security). Past versions of TLS were called SSL (Secure Sockets Layer). To convert your website to HTTPS, you will need to install a TLS/SSL certificate from a certificate authority (CA).

You can now obtain free, automatically renewing TSL/SSL certificates from [https://letsencrypt.org](Let's Encrypt). Before the holidays, I installed Let's Encrypt on Red Hat Enterprise Linux 6 servers running Apache. I will outline the steps in the rest of this article.

First, make sure that port 443 is accessible from the Internet. Check your iptables settings.

Next, install Certbot, the official Let's Encrypt client. Certbot instructions say to install the python-certbot RPM from the EPEL repository, but EPEL does not have the python-certbot RPM for RHEL 6. Instead, clone the Let's Encrypt Github repo into /usr/local. Then run certbot-auto from the cloned repository:

    $ cd /usr/local
    $ git clone https://github.com/letsencrypt/letsencrypt
    $ cd letsencrypt
    $ ./certbot-auto --apache

The above commands must be run as root.
    
When Certbot asks for domain names, enter any domain names associated with your server that have valid DNS A records. Select "Easy" or "Secure" on the next screen to determine whether to always forward http traffic to the https site. 

Certbot will create an ssl.conf file in /etc/httpd/conf.d with virtualhost information. Once certbot completes, restart Apache, and your new HTTPS website should be all set.
