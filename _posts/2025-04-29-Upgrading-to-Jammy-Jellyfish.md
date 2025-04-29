Last week, I finally upgraded my workstation from Ubuntu 20.04 to Ubuntu 22.04. Initially, I followed the steps at [https://ubuntu.com/tutorials/upgrading-ubuntu-desktop](https://ubuntu.com/tutorials/upgrading-ubuntu-desktop). but I got the following
errors in `/var/log/dist-upgrade/main.log`:

```
2025-04-22 11:30:45,993 DEBUG Marking 'ubuntu-desktop' for upgrade
2025-04-22 11:30:46,210 WARNING Can't mark 'ubuntu-desktop' for upgrade (E:Error, pkgProblemResolver::Resolve generated breaks, this may be caused by held packages.)
2025-04-22 11:30:46,258 DEBUG denylist expr 'update-manager$' matches 'update-manager'
2025-04-22 11:30:46,258 DEBUG The package 'update-manager' is marked for removal but it's in the removal deny list
2025-04-22 11:31:35,590 ERROR Dist-upgrade failed: 'The package 'update-manager' is marked for removal but it is in the removal deny list.'
2025-04-22 11:31:35,616 DEBUG abort called
```