# Version 19.02

* Acl "access_mam" for MAM in mod_muc
* Option "clear_archive_on_room_destroy" for mod_mam to prevent archive removal on room destroy
* Prevent room re-creation if room archive not empty and "clear_archive_on_room_destroy" is false
* Commands to list and destroy empty rooms by ejabberdctl ("rooms_empty_list" and "rooms_empty_destroy")

# Version 18.12

* MAM data store compression
* Proxy protocol support (http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt)
* MUC Self-Ping optimization (XEP-0410)
* Bookmarks conversion (XEP-0411)
