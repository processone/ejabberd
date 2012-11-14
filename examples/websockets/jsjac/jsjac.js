/* JSJaC - The JavaScript Jabber Client Library
 * Copyright (C) 2004-2008 Stefan Strigler
 *
 * JSJaC is licensed under the terms of the Mozilla Public License
 * version 1.1 or, at your option, under the terms of the GNU General
 * Public License version 2 or subsequent, or the terms of the GNU Lesser
 * General Public License version 2.1 or subsequent. 
 *
 * Please visit http://zeank.in-berlin.de/jsjac/ for details about JSJaC.
 */

JSJAC_HAVEKEYS = true;          // whether to use keys
JSJAC_NKEYS    = 16;            // number of keys to generate
JSJAC_INACTIVITY = 300;         // qnd hack to make suspend/resume 
                                    // work more smoothly with polling
JSJAC_ERR_COUNT = 10;           // number of retries in case of connection
                                    // errors

JSJAC_ALLOW_PLAIN = true;       // whether to allow plaintext logins

JSJAC_CHECKQUEUEINTERVAL = 1;   // msecs to poll send queue
JSJAC_CHECKINQUEUEINTERVAL = 1; // msecs to poll incoming queue
JSJAC_TIMERVAL = 2000;          // default polling interval
JSJAC_RETRYDELAY = 5000;        // msecs to wait before trying next request after error

// Options specific to HTTP Binding (BOSH)
JSJACHBC_MAX_HOLD = 1;          // default for number of connctions held by 
                                    // connection maanger 
JSJACHBC_MAX_WAIT = 300;        // default 'wait' param - how long an idle connection
                                    // should be held by connection manager

JSJACHBC_BOSH_VERSION  = "1.6";
JSJACHBC_USE_BOSH_VER  = true;

JSJACHBC_MAXPAUSE = 120;        // how long a suspend/resume cycle may take

/*** END CONFIG ***/


String.prototype.htmlEnc=function(){if(!this)
return this;var str=this.replace(/&/g,"&amp;");str=str.replace(/</g,"&lt;");str=str.replace(/>/g,"&gt;");str=str.replace(/\"/g,"&quot;");str=str.replace(/\n/g,"<br />");return str;};String.prototype.revertHtmlEnc=function(){if(!this)
return this;var str=this.replace(/&amp;/gi,'&');str=str.replace(/&lt;/gi,'<');str=str.replace(/&gt;/gi,'>');str=str.replace(/&quot;/gi,'\"');str=str.replace(/<br( )?(\/)?>/gi,'\n');return str;};Date.jab2date=function(ts){var date=new Date(Date.UTC(ts.substr(0,4),ts.substr(5,2)-1,ts.substr(8,2),ts.substr(11,2),ts.substr(14,2),ts.substr(17,2)));if(ts.substr(ts.length-6,1)!='Z'){var offset=new Date();offset.setTime(0);offset.setUTCHours(ts.substr(ts.length-5,2));offset.setUTCMinutes(ts.substr(ts.length-2,2));if(ts.substr(ts.length-6,1)=='+')
date.setTime(date.getTime()-offset.getTime());else if(ts.substr(ts.length-6,1)=='-')
date.setTime(date.getTime()+offset.getTime());}
return date;};Date.hrTime=function(ts){return Date.jab2date(ts).toLocaleString();};Date.prototype.jabberDate=function(){var padZero=function(i){if(i<10)return"0"+i;return i;};var jDate=this.getUTCFullYear()+"-";jDate+=padZero(this.getUTCMonth()+1)+"-";jDate+=padZero(this.getUTCDate())+"T";jDate+=padZero(this.getUTCHours())+":";jDate+=padZero(this.getUTCMinutes())+":";jDate+=padZero(this.getUTCSeconds())+"Z";return jDate;};Number.max=function(A,B){return(A>B)?A:B;};Number.min=function(A,B){return(A<B)?A:B;};if(window.XDomainRequest){window.ieXDRToXHR=function(window){"use strict";var XHR=window.XMLHttpRequest;window.XMLHttpRequest=function(){this.onreadystatechange=Object;this.xhr=null;this.xdr=null;this.readyState=0;this.status='';this.statusText=null;this.responseText=null;this.getResponseHeader=null;this.getAllResponseHeaders=null;this.setRequestHeader=null;this.abort=null;this.send=null;this.isxdr=false;var self=this;self.xdrLoadedBinded=function(){self.xdrLoaded();};self.xdrErrorBinded=function(){self.xdrError();};self.xdrProgressBinded=function(){self.xdrProgress();};self.xhrReadyStateChangedBinded=function(){self.xhrReadyStateChanged();};};XMLHttpRequest.prototype.open=function(method,url,asynch,user,pwd){if(url.indexOf(document.domain)<0&&url.indexOf('http-bind')>0){if(this.xdr===null){this.xdr=new window.XDomainRequest();}
this.isxdr=true;this.setXDRActive();this.xdr.open(method,url);}else{if(this.xhr===null){this.xhr=new XHR();}
this.isxdr=false;this.setXHRActive();this.xhr.open(method,url,asynch,user,pwd);}};XMLHttpRequest.prototype.xdrGetResponseHeader=function(name){if(name==='Content-Type'&&this.xdr.contentType>''){return this.xdr.contentType;}
return'';};XMLHttpRequest.prototype.xdrGetAllResponseHeaders=function(){return(this.xdr.contentType>'')?'Content-Type: '+this.xdr.contentType:'';};XMLHttpRequest.prototype.xdrSetRequestHeader=function(name,value){};XMLHttpRequest.prototype.xdrLoaded=function(){if(this.onreadystatechange!==null){this.readyState=4;this.status=200;this.statusText='OK';this.responseText=this.xdr.responseText;if(window.ActiveXObject){var doc=new ActiveXObject('Microsoft.XMLDOM');doc.async='false';doc.loadXML(this.responseText);this.responseXML=doc;}
this.onreadystatechange();}};XMLHttpRequest.prototype.xdrError=function(){if(this.onreadystatechange!==null){this.readyState=4;this.status=0;this.statusText='';this.responseText='';this.onreadystatechange();}};XMLHttpRequest.prototype.xdrProgress=function(){if(this.onreadystatechange!==null&&this.status!==3){this.readyState=3;this.status=3;this.statusText='';this.onreadystatechange();}};XMLHttpRequest.prototype.finalXDRRequest=function(){var xdr=this.xdr;delete xdr.onload;delete xdr.onerror;delete xdr.onprogress;};XMLHttpRequest.prototype.sendXDR=function(data){var xdr=this.xdr;xdr.onload=this.xdrLoadedBinded;xdr.onerror=this.xdr.ontimeout=this.xdrErrorBinded;xdr.onprogress=this.xdrProgressBinded;this.responseText=null;this.xdr.send(data);};XMLHttpRequest.prototype.abortXDR=function(){this.finalXDRRequest();this.xdr.abort();};XMLHttpRequest.prototype.setXDRActive=function(){this.send=this.sendXDR;this.abort=this.abortXDR;this.getResponseHeader=this.xdrGetResponseHeader;this.getAllResponseHeaders=this.xdrGetAllResponseHeaders;this.setRequestHeader=this.xdrSetRequestHeader;};XMLHttpRequest.prototype.xhrGetResponseHeader=function(name){return this.xhr.getResponseHeader(name);};XMLHttpRequest.prototype.xhrGetAllResponseHeaders=function(){return this.xhr.getAllResponseHeaders();};XMLHttpRequest.prototype.xhrSetRequestHeader=function(name,value){return this.xhr.setRequestHeader(name,value);};XMLHttpRequest.prototype.xhrReadyStateChanged=function(){if(this.onreadystatechange!==null&&this.readyState!==this.xhr.readyState){var xhr=this.xhr;this.readyState=xhr.readyState;if(this.readyState===4){this.status=xhr.status;this.statusText=xhr.statusText;this.responseText=xhr.responseText;this.responseXML=xhr.responseXML;}
this.onreadystatechange();}};XMLHttpRequest.prototype.finalXHRRequest=function(){delete this.xhr.onreadystatechange;};XMLHttpRequest.prototype.abortXHR=function(){this.finalXHRRequest();this.xhr.abort();};XMLHttpRequest.prototype.sendXHR=function(data){this.xhr.onreadystatechange=this.xhrReadyStateChangedBinded;this.xhr.send(data);};XMLHttpRequest.prototype.setXHRActive=function(){this.send=this.sendXHR;this.abort=this.abortXHR;this.getResponseHeader=this.xhrGetResponseHeader;this.getAllResponseHeaders=this.xhrGetAllResponseHeaders;this.setRequestHeader=this.xhrSetRequestHeader;};window.ieXDRToXHR=undefined;};window.ieXDRToXHR(window);}
var hexcase=0;var b64pad="=";var chrsz=8;function hex_sha1(s){return binb2hex(core_sha1(str2binb(s),s.length*chrsz));}
function b64_sha1(s){return binb2b64(core_sha1(str2binb(s),s.length*chrsz));}
function str_sha1(s){return binb2str(core_sha1(str2binb(s),s.length*chrsz));}
function hex_hmac_sha1(key,data){return binb2hex(core_hmac_sha1(key,data));}
function b64_hmac_sha1(key,data){return binb2b64(core_hmac_sha1(key,data));}
function str_hmac_sha1(key,data){return binb2str(core_hmac_sha1(key,data));}
function sha1_vm_test()
{return hex_sha1("abc")=="a9993e364706816aba3e25717850c26c9cd0d89d";}
function core_sha1(x,len)
{x[len>>5]|=0x80<<(24-len%32);x[((len+64>>9)<<4)+15]=len;var w=Array(80);var a=1732584193;var b=-271733879;var c=-1732584194;var d=271733878;var e=-1009589776;for(var i=0;i<x.length;i+=16)
{var olda=a;var oldb=b;var oldc=c;var oldd=d;var olde=e;for(var j=0;j<80;j++)
{if(j<16)w[j]=x[i+j];else w[j]=rol(w[j-3]^w[j-8]^w[j-14]^w[j-16],1);var t=safe_add(safe_add(rol(a,5),sha1_ft(j,b,c,d)),safe_add(safe_add(e,w[j]),sha1_kt(j)));e=d;d=c;c=rol(b,30);b=a;a=t;}
a=safe_add(a,olda);b=safe_add(b,oldb);c=safe_add(c,oldc);d=safe_add(d,oldd);e=safe_add(e,olde);}
return Array(a,b,c,d,e);}
function sha1_ft(t,b,c,d)
{if(t<20)return(b&c)|((~b)&d);if(t<40)return b^c^d;if(t<60)return(b&c)|(b&d)|(c&d);return b^c^d;}
function sha1_kt(t)
{return(t<20)?1518500249:(t<40)?1859775393:(t<60)?-1894007588:-899497514;}
function core_hmac_sha1(key,data)
{var bkey=str2binb(key);if(bkey.length>16)bkey=core_sha1(bkey,key.length*chrsz);var ipad=Array(16),opad=Array(16);for(var i=0;i<16;i++)
{ipad[i]=bkey[i]^0x36363636;opad[i]=bkey[i]^0x5C5C5C5C;}
var hash=core_sha1(ipad.concat(str2binb(data)),512+data.length*chrsz);return core_sha1(opad.concat(hash),512+160);}
function rol(num,cnt)
{return(num<<cnt)|(num>>>(32-cnt));}
function str2binb(str)
{var bin=Array();var mask=(1<<chrsz)-1;for(var i=0;i<str.length*chrsz;i+=chrsz)
bin[i>>5]|=(str.charCodeAt(i/chrsz)&mask)<<(32-chrsz-i%32);return bin;}
function binb2str(bin)
{var str="";var mask=(1<<chrsz)-1;for(var i=0;i<bin.length*32;i+=chrsz)
str+=String.fromCharCode((bin[i>>5]>>>(32-chrsz-i%32))&mask);return str;}
function binb2hex(binarray)
{var hex_tab=hexcase?"0123456789ABCDEF":"0123456789abcdef";var str="";for(var i=0;i<binarray.length*4;i++)
{str+=hex_tab.charAt((binarray[i>>2]>>((3-i%4)*8+4))&0xF)+
hex_tab.charAt((binarray[i>>2]>>((3-i%4)*8))&0xF);}
return str;}
function binb2b64(binarray)
{var tab="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";var str="";for(var i=0;i<binarray.length*4;i+=3)
{var triplet=(((binarray[i>>2]>>8*(3-i%4))&0xFF)<<16)|(((binarray[i+1>>2]>>8*(3-(i+1)%4))&0xFF)<<8)|((binarray[i+2>>2]>>8*(3-(i+2)%4))&0xFF);for(var j=0;j<4;j++)
{if(i*8+j*6>binarray.length*32)str+=b64pad;else str+=tab.charAt((triplet>>6*(3-j))&0x3F);}}
return str.replace(/AAA\=(\=*?)$/,'$1');}
function hex_md5(s){return binl2hex(core_md5(str2binl(s),s.length*chrsz));}
function b64_md5(s){return binl2b64(core_md5(str2binl(s),s.length*chrsz));}
function str_md5(s){return binl2str(core_md5(str2binl(s),s.length*chrsz));}
function hex_hmac_md5(key,data){return binl2hex(core_hmac_md5(key,data));}
function b64_hmac_md5(key,data){return binl2b64(core_hmac_md5(key,data));}
function str_hmac_md5(key,data){return binl2str(core_hmac_md5(key,data));}
function md5_vm_test()
{return hex_md5("abc")=="900150983cd24fb0d6963f7d28e17f72";}
function core_md5(x,len)
{x[len>>5]|=0x80<<((len)%32);x[(((len+64)>>>9)<<4)+14]=len;var a=1732584193;var b=-271733879;var c=-1732584194;var d=271733878;for(var i=0;i<x.length;i+=16)
{var olda=a;var oldb=b;var oldc=c;var oldd=d;a=md5_ff(a,b,c,d,x[i+0],7,-680876936);d=md5_ff(d,a,b,c,x[i+1],12,-389564586);c=md5_ff(c,d,a,b,x[i+2],17,606105819);b=md5_ff(b,c,d,a,x[i+3],22,-1044525330);a=md5_ff(a,b,c,d,x[i+4],7,-176418897);d=md5_ff(d,a,b,c,x[i+5],12,1200080426);c=md5_ff(c,d,a,b,x[i+6],17,-1473231341);b=md5_ff(b,c,d,a,x[i+7],22,-45705983);a=md5_ff(a,b,c,d,x[i+8],7,1770035416);d=md5_ff(d,a,b,c,x[i+9],12,-1958414417);c=md5_ff(c,d,a,b,x[i+10],17,-42063);b=md5_ff(b,c,d,a,x[i+11],22,-1990404162);a=md5_ff(a,b,c,d,x[i+12],7,1804603682);d=md5_ff(d,a,b,c,x[i+13],12,-40341101);c=md5_ff(c,d,a,b,x[i+14],17,-1502002290);b=md5_ff(b,c,d,a,x[i+15],22,1236535329);a=md5_gg(a,b,c,d,x[i+1],5,-165796510);d=md5_gg(d,a,b,c,x[i+6],9,-1069501632);c=md5_gg(c,d,a,b,x[i+11],14,643717713);b=md5_gg(b,c,d,a,x[i+0],20,-373897302);a=md5_gg(a,b,c,d,x[i+5],5,-701558691);d=md5_gg(d,a,b,c,x[i+10],9,38016083);c=md5_gg(c,d,a,b,x[i+15],14,-660478335);b=md5_gg(b,c,d,a,x[i+4],20,-405537848);a=md5_gg(a,b,c,d,x[i+9],5,568446438);d=md5_gg(d,a,b,c,x[i+14],9,-1019803690);c=md5_gg(c,d,a,b,x[i+3],14,-187363961);b=md5_gg(b,c,d,a,x[i+8],20,1163531501);a=md5_gg(a,b,c,d,x[i+13],5,-1444681467);d=md5_gg(d,a,b,c,x[i+2],9,-51403784);c=md5_gg(c,d,a,b,x[i+7],14,1735328473);b=md5_gg(b,c,d,a,x[i+12],20,-1926607734);a=md5_hh(a,b,c,d,x[i+5],4,-378558);d=md5_hh(d,a,b,c,x[i+8],11,-2022574463);c=md5_hh(c,d,a,b,x[i+11],16,1839030562);b=md5_hh(b,c,d,a,x[i+14],23,-35309556);a=md5_hh(a,b,c,d,x[i+1],4,-1530992060);d=md5_hh(d,a,b,c,x[i+4],11,1272893353);c=md5_hh(c,d,a,b,x[i+7],16,-155497632);b=md5_hh(b,c,d,a,x[i+10],23,-1094730640);a=md5_hh(a,b,c,d,x[i+13],4,681279174);d=md5_hh(d,a,b,c,x[i+0],11,-358537222);c=md5_hh(c,d,a,b,x[i+3],16,-722521979);b=md5_hh(b,c,d,a,x[i+6],23,76029189);a=md5_hh(a,b,c,d,x[i+9],4,-640364487);d=md5_hh(d,a,b,c,x[i+12],11,-421815835);c=md5_hh(c,d,a,b,x[i+15],16,530742520);b=md5_hh(b,c,d,a,x[i+2],23,-995338651);a=md5_ii(a,b,c,d,x[i+0],6,-198630844);d=md5_ii(d,a,b,c,x[i+7],10,1126891415);c=md5_ii(c,d,a,b,x[i+14],15,-1416354905);b=md5_ii(b,c,d,a,x[i+5],21,-57434055);a=md5_ii(a,b,c,d,x[i+12],6,1700485571);d=md5_ii(d,a,b,c,x[i+3],10,-1894986606);c=md5_ii(c,d,a,b,x[i+10],15,-1051523);b=md5_ii(b,c,d,a,x[i+1],21,-2054922799);a=md5_ii(a,b,c,d,x[i+8],6,1873313359);d=md5_ii(d,a,b,c,x[i+15],10,-30611744);c=md5_ii(c,d,a,b,x[i+6],15,-1560198380);b=md5_ii(b,c,d,a,x[i+13],21,1309151649);a=md5_ii(a,b,c,d,x[i+4],6,-145523070);d=md5_ii(d,a,b,c,x[i+11],10,-1120210379);c=md5_ii(c,d,a,b,x[i+2],15,718787259);b=md5_ii(b,c,d,a,x[i+9],21,-343485551);a=safe_add(a,olda);b=safe_add(b,oldb);c=safe_add(c,oldc);d=safe_add(d,oldd);}
return Array(a,b,c,d);}
function md5_cmn(q,a,b,x,s,t)
{return safe_add(bit_rol(safe_add(safe_add(a,q),safe_add(x,t)),s),b);}
function md5_ff(a,b,c,d,x,s,t)
{return md5_cmn((b&c)|((~b)&d),a,b,x,s,t);}
function md5_gg(a,b,c,d,x,s,t)
{return md5_cmn((b&d)|(c&(~d)),a,b,x,s,t);}
function md5_hh(a,b,c,d,x,s,t)
{return md5_cmn(b^c^d,a,b,x,s,t);}
function md5_ii(a,b,c,d,x,s,t)
{return md5_cmn(c^(b|(~d)),a,b,x,s,t);}
function core_hmac_md5(key,data)
{var bkey=str2binl(key);if(bkey.length>16)bkey=core_md5(bkey,key.length*chrsz);var ipad=Array(16),opad=Array(16);for(var i=0;i<16;i++)
{ipad[i]=bkey[i]^0x36363636;opad[i]=bkey[i]^0x5C5C5C5C;}
var hash=core_md5(ipad.concat(str2binl(data)),512+data.length*chrsz);return core_md5(opad.concat(hash),512+128);}
function safe_add(x,y)
{var lsw=(x&0xFFFF)+(y&0xFFFF);var msw=(x>>16)+(y>>16)+(lsw>>16);return(msw<<16)|(lsw&0xFFFF);}
function bit_rol(num,cnt)
{return(num<<cnt)|(num>>>(32-cnt));}
function str2binl(str)
{var bin=Array();var mask=(1<<chrsz)-1;for(var i=0;i<str.length*chrsz;i+=chrsz)
bin[i>>5]|=(str.charCodeAt(i/chrsz)&mask)<<(i%32);return bin;}
function binl2str(bin)
{var str="";var mask=(1<<chrsz)-1;for(var i=0;i<bin.length*32;i+=chrsz)
str+=String.fromCharCode((bin[i>>5]>>>(i%32))&mask);return str;}
function binl2hex(binarray)
{var hex_tab=hexcase?"0123456789ABCDEF":"0123456789abcdef";var str="";for(var i=0;i<binarray.length*4;i++)
{str+=hex_tab.charAt((binarray[i>>2]>>((i%4)*8+4))&0xF)+
hex_tab.charAt((binarray[i>>2]>>((i%4)*8))&0xF);}
return str;}
function binl2b64(binarray)
{var tab="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";var str="";for(var i=0;i<binarray.length*4;i+=3)
{var triplet=(((binarray[i>>2]>>8*(i%4))&0xFF)<<16)|(((binarray[i+1>>2]>>8*((i+1)%4))&0xFF)<<8)|((binarray[i+2>>2]>>8*((i+2)%4))&0xFF);for(var j=0;j<4;j++)
{if(i*8+j*6>binarray.length*32)str+=b64pad;else str+=tab.charAt((triplet>>6*(3-j))&0x3F);}}
return str;}
function utf8t2d(t)
{t=t.replace(/\r\n/g,"\n");var d=new Array;var test=String.fromCharCode(237);if(test.charCodeAt(0)<0)
for(var n=0;n<t.length;n++)
{var c=t.charCodeAt(n);if(c>0)
d[d.length]=c;else{d[d.length]=(((256+c)>>6)|192);d[d.length]=(((256+c)&63)|128);}}
else
for(var n=0;n<t.length;n++)
{var c=t.charCodeAt(n);if(c<128)
d[d.length]=c;else if((c>127)&&(c<2048)){d[d.length]=((c>>6)|192);d[d.length]=((c&63)|128);}
else{d[d.length]=((c>>12)|224);d[d.length]=(((c>>6)&63)|128);d[d.length]=((c&63)|128);}}
return d;}
function utf8d2t(d)
{var r=new Array;var i=0;while(i<d.length)
{if(d[i]<128){r[r.length]=String.fromCharCode(d[i]);i++;}
else if((d[i]>191)&&(d[i]<224)){r[r.length]=String.fromCharCode(((d[i]&31)<<6)|(d[i+1]&63));i+=2;}
else{r[r.length]=String.fromCharCode(((d[i]&15)<<12)|((d[i+1]&63)<<6)|(d[i+2]&63));i+=3;}}
return r.join("");}
function b64arrays(){var b64s='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';b64=new Array();f64=new Array();for(var i=0;i<b64s.length;i++){b64[i]=b64s.charAt(i);f64[b64s.charAt(i)]=i;}}
function b64d2t(d){var r=new Array;var i=0;var dl=d.length;if((dl%3)==1){d[d.length]=0;d[d.length]=0;}
if((dl%3)==2)
d[d.length]=0;while(i<d.length)
{r[r.length]=b64[d[i]>>2];r[r.length]=b64[((d[i]&3)<<4)|(d[i+1]>>4)];r[r.length]=b64[((d[i+1]&15)<<2)|(d[i+2]>>6)];r[r.length]=b64[d[i+2]&63];i+=3;}
if((dl%3)==1)
r[r.length-1]=r[r.length-2]="=";if((dl%3)==2)
r[r.length-1]="=";var t=r.join("");return t;}
function b64t2d(t){var d=new Array;var i=0;t=t.replace(/\n|\r/g,"");t=t.replace(/=/g,"");while(i<t.length)
{d[d.length]=(f64[t.charAt(i)]<<2)|(f64[t.charAt(i+1)]>>4);d[d.length]=(((f64[t.charAt(i+1)]&15)<<4)|(f64[t.charAt(i+2)]>>2));d[d.length]=(((f64[t.charAt(i+2)]&3)<<6)|(f64[t.charAt(i+3)]));i+=4;}
if(t.length%4==2)
d=d.slice(0,d.length-2);if(t.length%4==3)
d=d.slice(0,d.length-1);return d;}
if(typeof(atob)=='undefined'||typeof(btoa)=='undefined')
b64arrays();if(typeof(atob)=='undefined'){b64decode=function(s){return utf8d2t(b64t2d(s));}}else{b64decode=function(s){return decodeURIComponent(escape(atob(s)));}}
if(typeof(btoa)=='undefined'){b64encode=function(s){return b64d2t(utf8t2d(s));}}else{b64encode=function(s){return btoa(unescape(encodeURIComponent(s)));}}
function cnonce(size){var tab="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";var cnonce='';for(var i=0;i<size;i++){cnonce+=tab.charAt(Math.round(Math.random(new Date().getTime())*(tab.length-1)));}
return cnonce;}
function JSJaCJSON(){}
JSJaCJSON.toString=function(obj){var m={'\b':'\\b','\t':'\\t','\n':'\\n','\f':'\\f','\r':'\\r','"':'\\"','\\':'\\\\'},s={array:function(x){var a=['['],b,f,i,l=x.length,v;for(i=0;i<l;i+=1){v=x[i];f=s[typeof v];if(f){try{v=f(v);if(typeof v=='string'){if(b){a[a.length]=',';}
a[a.length]=v;b=true;}}catch(e){}}}
a[a.length]=']';return a.join('');},'boolean':function(x){return String(x);},'null':function(x){return"null";},number:function(x){return isFinite(x)?String(x):'null';},object:function(x){if(x){if(x instanceof Array){return s.array(x);}
var a=['{'],b,f,i,v;for(i in x){if(x.hasOwnProperty(i)){v=x[i];f=s[typeof v];if(f){try{v=f(v);if(typeof v=='string'){if(b){a[a.length]=',';}
a.push(s.string(i),':',v);b=true;}}catch(e){}}}}
a[a.length]='}';return a.join('');}
return'null';},string:function(x){if(/["\\\x00-\x1f]/.test(x)){x=x.replace(/([\x00-\x1f\\"])/g,function(a,b){var c=m[b];if(c){return c;}
c=b.charCodeAt();return'\\u00'+
Math.floor(c/16).toString(16)+
(c%16).toString(16);});}
return'"'+x+'"';}};switch(typeof(obj)){case'object':return s.object(obj);case'array':return s.array(obj);}};JSJaCJSON.parse=function(str){try{return!(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(str.replace(/"(\\.|[^"\\])*"/g,'')))&&eval('('+str+')');}catch(e){return false;}};function XmlHttp(){}
XmlHttp.create=function(){try{if(window.XMLHttpRequest){var req=new XMLHttpRequest();if(req.readyState===null){req.readyState=1;req.addEventListener("load",function(){req.readyState=4;if(typeof req.onreadystatechange=="function")
req.onreadystatechange();},false);}
return req;}
if(window.ActiveXObject){return new ActiveXObject(XmlHttp.getPrefix()+".XmlHttp");}}
catch(ex){}
throw new Error("Your browser does not support XmlHttp objects");};XmlHttp.getPrefix=function(){if(XmlHttp.prefix)
return XmlHttp.prefix;var prefixes=["MSXML2","Microsoft","MSXML","MSXML3"];var o;for(var i=0;i<prefixes.length;i++){try{o=new ActiveXObject(prefixes[i]+".XmlHttp");return XmlHttp.prefix=prefixes[i];}
catch(ex){}}
throw new Error("Could not find an installed XML parser");};function XmlDocument(){}
XmlDocument.create=function(name,ns){name=name||'foo';ns=ns||'';try{var doc;if(document.implementation&&document.implementation.createDocument){doc=document.implementation.createDocument(ns,name,null);if(doc.readyState===null){doc.readyState=1;doc.addEventListener("load",function(){doc.readyState=4;if(typeof doc.onreadystatechange=="function")
doc.onreadystatechange();},false);}}else if(window.ActiveXObject){doc=new ActiveXObject(XmlDocument.getPrefix()+".DomDocument");}
if(!doc.documentElement||doc.documentElement.tagName!=name||(doc.documentElement.namespaceURI&&doc.documentElement.namespaceURI!=ns)){try{if(ns!=='')
doc.appendChild(doc.createElement(name)).setAttribute('xmlns',ns);else
doc.appendChild(doc.createElement(name));}catch(dex){doc=document.implementation.createDocument(ns,name,null);if(doc.documentElement===null)
doc.appendChild(doc.createElement(name));if(ns!==''&&doc.documentElement.getAttribute('xmlns')!=ns){doc.documentElement.setAttribute('xmlns',ns);}}}
return doc;}
catch(ex){}
throw new Error("Your browser does not support XmlDocument objects");};XmlDocument.getPrefix=function(){if(XmlDocument.prefix)
return XmlDocument.prefix;var prefixes=["MSXML2","Microsoft","MSXML","MSXML3"];var o;for(var i=0;i<prefixes.length;i++){try{o=new ActiveXObject(prefixes[i]+".DomDocument");return XmlDocument.prefix=prefixes[i];}
catch(ex){}}
throw new Error("Could not find an installed XML parser");};if(typeof(Document)!='undefined'&&window.DOMParser){Document.prototype.loadXML=function(s){var doc2=(new DOMParser()).parseFromString(s,"text/xml");while(this.hasChildNodes())
this.removeChild(this.lastChild);for(var i=0;i<doc2.childNodes.length;i++){this.appendChild(this.importNode(doc2.childNodes[i],true));}};}
if(window.XMLSerializer&&window.Node&&Node.prototype&&Node.prototype.__defineGetter__){XMLDocument.prototype.__defineGetter__("xml",function(){return(new XMLSerializer()).serializeToString(this);});Document.prototype.__defineGetter__("xml",function(){return(new XMLSerializer()).serializeToString(this);});Node.prototype.__defineGetter__("xml",function(){return(new XMLSerializer()).serializeToString(this);});}
var JSJaCBuilder={buildNode:function(doc,elementName){var element,ns=arguments[4];if(arguments[2])
if(JSJaCBuilder._isStringOrNumber(arguments[2])||(arguments[2]instanceof Array)){element=this._createElement(doc,elementName,ns);JSJaCBuilder._children(doc,element,arguments[2]);}else{ns=arguments[2]['xmlns']||ns;element=this._createElement(doc,elementName,ns);for(var attr in arguments[2]){if(arguments[2].hasOwnProperty(attr)&&attr!='xmlns')
element.setAttribute(attr,arguments[2][attr]);}}
else
element=this._createElement(doc,elementName,ns);if(arguments[3])
JSJaCBuilder._children(doc,element,arguments[3],ns);return element;},_createElement:function(doc,elementName,ns){try{if(ns)
return doc.createElementNS(ns,elementName);}catch(ex){}
var el=doc.createElement(elementName);if(ns)
el.setAttribute("xmlns",ns);return el;},_text:function(doc,text){return doc.createTextNode(text);},_children:function(doc,element,children,ns){if(typeof children=='object'){for(var i in children){if(children.hasOwnProperty(i)){var e=children[i];if(typeof e=='object'){if(e instanceof Array){var node=JSJaCBuilder.buildNode(doc,e[0],e[1],e[2],ns);element.appendChild(node);}else{element.appendChild(e);}}else{if(JSJaCBuilder._isStringOrNumber(e)){element.appendChild(JSJaCBuilder._text(doc,e));}}}}}else{if(JSJaCBuilder._isStringOrNumber(children)){element.appendChild(JSJaCBuilder._text(doc,children));}}},_attributes:function(attributes){var attrs=[];for(var attribute in attributes)
if(attributes.hasOwnProperty(attribute))
attrs.push(attribute+'="'+attributes[attribute].toString().htmlEnc()+'"');return attrs.join(" ");},_isStringOrNumber:function(param){return(typeof param=='string'||typeof param=='number');}};var NS_DISCO_ITEMS="http://jabber.org/protocol/disco#items";var NS_DISCO_INFO="http://jabber.org/protocol/disco#info";var NS_VCARD="vcard-temp";var NS_AUTH="jabber:iq:auth";var NS_AUTH_ERROR="jabber:iq:auth:error";var NS_REGISTER="jabber:iq:register";var NS_SEARCH="jabber:iq:search";var NS_ROSTER="jabber:iq:roster";var NS_PRIVACY="jabber:iq:privacy";var NS_PRIVATE="jabber:iq:private";var NS_VERSION="jabber:iq:version";var NS_TIME="jabber:iq:time";var NS_TIME_NEW="urn:xmpp:time";var NS_LAST="jabber:iq:last";var NS_XDATA="jabber:x:data";var NS_IQDATA="jabber:iq:data";var NS_DELAY="jabber:x:delay";var NS_DELAY_NEW="urn:xmpp:delay";var NS_EXPIRE="jabber:x:expire";var NS_EVENT="jabber:x:event";var NS_XCONFERENCE="jabber:x:conference";var NS_PING="urn:xmpp:ping";var NS_CHAT_STATES="http://jabber.org/protocol/chatstates";var NS_STATS="http://jabber.org/protocol/stats";var NS_MUC="http://jabber.org/protocol/muc";var NS_MUC_USER="http://jabber.org/protocol/muc#user";var NS_MUC_ADMIN="http://jabber.org/protocol/muc#admin";var NS_MUC_OWNER="http://jabber.org/protocol/muc#owner";var NS_PUBSUB="http://jabber.org/protocol/pubsub";var NS_PUBSUB_EVENT="http://jabber.org/protocol/pubsub#event";var NS_PUBSUB_OWNER="http://jabber.org/protocol/pubsub#owner";var NS_PUBSUB_NMI="http://jabber.org/protocol/pubsub#node-meta-info";var NS_COMMANDS="http://jabber.org/protocol/commands";var NS_STREAM="http://etherx.jabber.org/streams";var NS_CLIENT="jabber:client";var NS_BOSH="http://jabber.org/protocol/httpbind";var NS_XBOSH="urn:xmpp:xbosh";var NS_STANZAS="urn:ietf:params:xml:ns:xmpp-stanzas";var NS_STREAMS="urn:ietf:params:xml:ns:xmpp-streams";var NS_TLS="urn:ietf:params:xml:ns:xmpp-tls";var NS_SASL="urn:ietf:params:xml:ns:xmpp-sasl";var NS_SESSION="urn:ietf:params:xml:ns:xmpp-session";var NS_BIND="urn:ietf:params:xml:ns:xmpp-bind";var NS_FEATURE_IQAUTH="http://jabber.org/features/iq-auth";var NS_FEATURE_IQREGISTER="http://jabber.org/features/iq-register";var NS_FEATURE_COMPRESS="http://jabber.org/features/compress";var NS_COMPRESS="http://jabber.org/protocol/compress";function STANZA_ERROR(code,type,cond){if(window==this)
return new STANZA_ERROR(code,type,cond);this.code=code;this.type=type;this.cond=cond;}
var ERR_BAD_REQUEST=STANZA_ERROR("400","modify","bad-request");var ERR_CONFLICT=STANZA_ERROR("409","cancel","conflict");var ERR_FEATURE_NOT_IMPLEMENTED=STANZA_ERROR("501","cancel","feature-not-implemented");var ERR_FORBIDDEN=STANZA_ERROR("403","auth","forbidden");var ERR_GONE=STANZA_ERROR("302","modify","gone");var ERR_INTERNAL_SERVER_ERROR=STANZA_ERROR("500","wait","internal-server-error");var ERR_ITEM_NOT_FOUND=STANZA_ERROR("404","cancel","item-not-found");var ERR_JID_MALFORMED=STANZA_ERROR("400","modify","jid-malformed");var ERR_NOT_ACCEPTABLE=STANZA_ERROR("406","modify","not-acceptable");var ERR_NOT_ALLOWED=STANZA_ERROR("405","cancel","not-allowed");var ERR_NOT_AUTHORIZED=STANZA_ERROR("401","auth","not-authorized");var ERR_PAYMENT_REQUIRED=STANZA_ERROR("402","auth","payment-required");var ERR_RECIPIENT_UNAVAILABLE=STANZA_ERROR("404","wait","recipient-unavailable");var ERR_REDIRECT=STANZA_ERROR("302","modify","redirect");var ERR_REGISTRATION_REQUIRED=STANZA_ERROR("407","auth","registration-required");var ERR_REMOTE_SERVER_NOT_FOUND=STANZA_ERROR("404","cancel","remote-server-not-found");var ERR_REMOTE_SERVER_TIMEOUT=STANZA_ERROR("504","wait","remote-server-timeout");var ERR_RESOURCE_CONSTRAINT=STANZA_ERROR("500","wait","resource-constraint");var ERR_SERVICE_UNAVAILABLE=STANZA_ERROR("503","cancel","service-unavailable");var ERR_SUBSCRIPTION_REQUIRED=STANZA_ERROR("407","auth","subscription-required");var ERR_UNEXPECTED_REQUEST=STANZA_ERROR("400","wait","unexpected-request");function JSJaCConsoleLogger(level){this.level=level||4;this.start=function(){};this.log=function(msg,level){level=level||0;if(level>this.level)
return;if(typeof(console)=='undefined')
return;try{switch(level){case 0:console.warn(msg);break;case 1:console.error(msg);break;case 2:console.info(msg);break;case 4:console.debug(msg);break;default:console.log(msg);break;}}catch(e){try{console.log(msg);}catch(e){}}};this.setLevel=function(level){this.level=level;return this;};this.getLevel=function(){return this.level;};}
function JSJaCCookie(name,value,secs,domain,path)
{if(window==this)
return new JSJaCCookie(name,value,secs,domain,path);this.name=name;this.value=value;this.secs=secs;this.domain=domain;this.path=path;this.write=function(){var expires;if(this.secs){var date=new Date();date.setTime(date.getTime()+(this.secs*1000));expires="; expires="+date.toGMTString();}else
expires="";var domain=this.domain?"; domain="+this.domain:"";var path=this.path?"; path="+this.path:"; path=/";document.cookie=this.getName()+"="+JSJaCCookie._escape(this.getValue())+
expires+
domain+
path;};this.erase=function(){var c=new JSJaCCookie(this.getName(),"",-1);c.write();};this.getName=function(){return this.name;};this.setName=function(name){this.name=name;return this;};this.getValue=function(){return this.value;};this.setValue=function(value){this.value=value;return this;};this.setDomain=function(domain){this.domain=domain;return this;};this.setPath=function(path){this.path=path;return this;};}
JSJaCCookie.read=function(name){var nameEQ=name+"=";var ca=document.cookie.split(';');for(var i=0;i<ca.length;i++){var c=ca[i];while(c.charAt(0)==' ')c=c.substring(1,c.length);if(c.indexOf(nameEQ)==0)
return new JSJaCCookie(name,JSJaCCookie._unescape(c.substring(nameEQ.length,c.length)));}
throw new JSJaCCookieException("Cookie not found");};JSJaCCookie.get=function(name){return JSJaCCookie.read(name).getValue();};JSJaCCookie.remove=function(name){JSJaCCookie.read(name).erase();};JSJaCCookie._escape=function(str){return str.replace(/;/g,"%3AB");}
JSJaCCookie._unescape=function(str){return str.replace(/%3AB/g,";");}
function JSJaCCookieException(msg){this.message=msg;this.name="CookieException";}
function JSJaCError(code,type,condition){var xmldoc=XmlDocument.create("error","jsjac");xmldoc.documentElement.setAttribute('code',code);xmldoc.documentElement.setAttribute('type',type);if(condition)
xmldoc.documentElement.appendChild(xmldoc.createElement(condition)).setAttribute('xmlns',NS_STANZAS);return xmldoc.documentElement;}
var JSJACJID_FORBIDDEN=['"',' ','&','\'','/',':','<','>','@'];function JSJaCJID(jid){this._node='';this._domain='';this._resource='';if(typeof(jid)=='string'){if(jid.indexOf('@')!=-1){this.setNode(jid.substring(0,jid.indexOf('@')));jid=jid.substring(jid.indexOf('@')+1);}
if(jid.indexOf('/')!=-1){this.setResource(jid.substring(jid.indexOf('/')+1));jid=jid.substring(0,jid.indexOf('/'));}
this.setDomain(jid);}else{this.setNode(jid.node);this.setDomain(jid.domain);this.setResource(jid.resource);}}
JSJaCJID.prototype.getBareJID=function(){return this.getNode()+'@'+this.getDomain();};JSJaCJID.prototype.getNode=function(){return this._node;};JSJaCJID.prototype.getDomain=function(){return this._domain;};JSJaCJID.prototype.getResource=function(){return this._resource;};JSJaCJID.prototype.setNode=function(node){JSJaCJID._checkNodeName(node);this._node=node||'';return this;};JSJaCJID.prototype.setDomain=function(domain){if(!domain||domain=='')
throw new JSJaCJIDInvalidException("domain name missing");JSJaCJID._checkNodeName(domain);this._domain=domain;return this;};JSJaCJID.prototype.setResource=function(resource){this._resource=resource||'';return this;};JSJaCJID.prototype.toString=function(){var jid='';if(this.getNode()&&this.getNode()!='')
jid=this.getNode()+'@';jid+=this.getDomain();if(this.getResource()&&this.getResource()!="")
jid+='/'+this.getResource();return jid;};JSJaCJID.prototype.removeResource=function(){return this.setResource();};JSJaCJID.prototype.clone=function(){return new JSJaCJID(this.toString());};JSJaCJID.prototype.isEntity=function(jid){if(typeof jid=='string')
jid=(new JSJaCJID(jid));jid.removeResource();return(this.clone().removeResource().toString()===jid.toString());};JSJaCJID._checkNodeName=function(nodeprep){if(!nodeprep||nodeprep=='')
return;for(var i=0;i<JSJACJID_FORBIDDEN.length;i++){if(nodeprep.indexOf(JSJACJID_FORBIDDEN[i])!=-1){throw new JSJaCJIDInvalidException("forbidden char in nodename: "+JSJACJID_FORBIDDEN[i]);}}};function JSJaCJIDInvalidException(message){this.message=message;this.name="JSJaCJIDInvalidException";}
function JSJaCKeys(func,oDbg){var seed=Math.random();this._k=new Array();this._k[0]=seed.toString();if(oDbg)
this.oDbg=oDbg;else{this.oDbg={};this.oDbg.log=function(){};}
if(func){for(var i=1;i<JSJAC_NKEYS;i++){this._k[i]=func(this._k[i-1]);oDbg.log(i+": "+this._k[i],4);}}
this._indexAt=JSJAC_NKEYS-1;this.getKey=function(){return this._k[this._indexAt--];};this.lastKey=function(){return(this._indexAt==0);};this.size=function(){return this._k.length;};this._getSuspendVars=function(){return('_k,_indexAt').split(',');};}
var JSJACPACKET_USE_XMLNS=true;function JSJaCPacket(name){this.name=name;if(typeof(JSJACPACKET_USE_XMLNS)!='undefined'&&JSJACPACKET_USE_XMLNS)
this.doc=XmlDocument.create(name,NS_CLIENT);else
this.doc=XmlDocument.create(name,'');}
JSJaCPacket.prototype.pType=function(){return this.name;};JSJaCPacket.prototype.getDoc=function(){return this.doc;};JSJaCPacket.prototype.getNode=function(){if(this.getDoc()&&this.getDoc().documentElement)
return this.getDoc().documentElement;else
return null;};JSJaCPacket.prototype.setTo=function(to){if(!to||to=='')
this.getNode().removeAttribute('to');else if(typeof(to)=='string')
this.getNode().setAttribute('to',to);else
this.getNode().setAttribute('to',to.toString());return this;};JSJaCPacket.prototype.setFrom=function(from){if(!from||from=='')
this.getNode().removeAttribute('from');else if(typeof(from)=='string')
this.getNode().setAttribute('from',from);else
this.getNode().setAttribute('from',from.toString());return this;};JSJaCPacket.prototype.setID=function(id){if(!id||id=='')
this.getNode().removeAttribute('id');else
this.getNode().setAttribute('id',id);return this;};JSJaCPacket.prototype.setType=function(type){if(!type||type=='')
this.getNode().removeAttribute('type');else
this.getNode().setAttribute('type',type);return this;};JSJaCPacket.prototype.setXMLLang=function(xmllang){if(!xmllang||xmllang=='')
this.getNode().removeAttribute('xml:lang');else
this.getNode().setAttribute('xml:lang',xmllang);return this;};JSJaCPacket.prototype.getTo=function(){return this.getNode().getAttribute('to');};JSJaCPacket.prototype.getFrom=function(){return this.getNode().getAttribute('from');};JSJaCPacket.prototype.getToJID=function(){return new JSJaCJID(this.getTo());};JSJaCPacket.prototype.getFromJID=function(){return new JSJaCJID(this.getFrom());};JSJaCPacket.prototype.getID=function(){return this.getNode().getAttribute('id');};JSJaCPacket.prototype.getType=function(){return this.getNode().getAttribute('type');};JSJaCPacket.prototype.getXMLLang=function(){return this.getNode().getAttribute('xml:lang');};JSJaCPacket.prototype.getXMLNS=function(){return this.getNode().namespaceURI||this.getNode().getAttribute('xmlns');};JSJaCPacket.prototype.getChild=function(name,ns){if(!this.getNode()){return null;}
name=name||'*';ns=ns||'*';if(this.getNode().getElementsByTagNameNS){return this.getNode().getElementsByTagNameNS(ns,name).item(0);}
var nodes=this.getNode().getElementsByTagName(name);if(ns!='*'){for(var i=0;i<nodes.length;i++){if(nodes.item(i).namespaceURI==ns||nodes.item(i).getAttribute('xmlns')==ns){return nodes.item(i);}}}else{return nodes.item(0);}
return null;};JSJaCPacket.prototype.getChildVal=function(name,ns){var node=this.getChild(name,ns);var ret='';if(node&&node.hasChildNodes()){for(var i=0;i<node.childNodes.length;i++)
if(node.childNodes.item(i).nodeValue)
ret+=node.childNodes.item(i).nodeValue;}
return ret;};JSJaCPacket.prototype.clone=function(){return JSJaCPacket.wrapNode(this.getNode());};JSJaCPacket.prototype.isError=function(){return(this.getType()=='error');};JSJaCPacket.prototype.errorReply=function(stanza_error){var rPacket=this.clone();rPacket.setTo(this.getFrom());rPacket.setFrom();rPacket.setType('error');rPacket.appendNode('error',{code:stanza_error.code,type:stanza_error.type},[[stanza_error.cond,{xmlns:NS_STANZAS}]]);return rPacket;};JSJaCPacket.prototype.xml=typeof XMLSerializer!='undefined'?function(){var r=(new XMLSerializer()).serializeToString(this.getNode());if(typeof(r)=='undefined')
r=(new XMLSerializer()).serializeToString(this.doc);return r;}:function(){return this.getDoc().xml;};JSJaCPacket.prototype._getAttribute=function(attr){return this.getNode().getAttribute(attr);};if(document.ELEMENT_NODE==null){document.ELEMENT_NODE=1;document.ATTRIBUTE_NODE=2;document.TEXT_NODE=3;document.CDATA_SECTION_NODE=4;document.ENTITY_REFERENCE_NODE=5;document.ENTITY_NODE=6;document.PROCESSING_INSTRUCTION_NODE=7;document.COMMENT_NODE=8;document.DOCUMENT_NODE=9;document.DOCUMENT_TYPE_NODE=10;document.DOCUMENT_FRAGMENT_NODE=11;document.NOTATION_NODE=12;}
JSJaCPacket.prototype._importNode=function(node,allChildren){switch(node.nodeType){case document.ELEMENT_NODE:var newNode;if(this.getDoc().createElementNS){newNode=this.getDoc().createElementNS(node.namespaceURI,node.nodeName);}else{newNode=this.getDoc().createElement(node.nodeName);}
var i,il;if(node.attributes&&node.attributes.length>0)
for(i=0,il=node.attributes.length;i<il;i++){var attr=node.attributes.item(i);if(attr.nodeName=='xmlns'&&(newNode.getAttribute('xmlns')!==null||newNode.namespaceURI)){continue;}
if(newNode.setAttributeNS&&attr.namespaceURI){newNode.setAttributeNS(attr.namespaceURI,attr.name,attr.value);}else{newNode.setAttribute(attr.name,attr.value);}}
if(allChildren&&node.childNodes&&node.childNodes.length>0){for(i=0,il=node.childNodes.length;i<il;i++){newNode.appendChild(this._importNode(node.childNodes.item(i),allChildren));}}
return newNode;case document.TEXT_NODE:case document.CDATA_SECTION_NODE:case document.COMMENT_NODE:return this.getDoc().createTextNode(node.nodeValue);}};JSJaCPacket.prototype._setChildNode=function(nodeName,nodeValue){var aNode=this.getChild(nodeName);var tNode=this.getDoc().createTextNode(nodeValue);if(aNode)
try{aNode.replaceChild(tNode,aNode.firstChild);}catch(e){}
else{try{aNode=this.getDoc().createElementNS(this.getNode().namespaceURI,nodeName);}catch(ex){aNode=this.getDoc().createElement(nodeName);}
this.getNode().appendChild(aNode);aNode.appendChild(tNode);}
return aNode;};JSJaCPacket.prototype.buildNode=function(elementName){return JSJaCBuilder.buildNode(this.getDoc(),elementName,arguments[1],arguments[2],arguments[3]);};JSJaCPacket.prototype.appendNode=function(element){if(typeof element=='object'){return this.getNode().appendChild(element);}else{return this.getNode().appendChild(this.buildNode(element,arguments[1],arguments[2],this.getNode().namespaceURI));}};function JSJaCPresence(){this.base=JSJaCPacket;this.base('presence');}
JSJaCPresence.prototype=new JSJaCPacket;JSJaCPresence.prototype.setStatus=function(status){this._setChildNode("status",status);return this;};JSJaCPresence.prototype.setShow=function(show){if(show=='chat'||show=='away'||show=='xa'||show=='dnd')
this._setChildNode("show",show);return this;};JSJaCPresence.prototype.setPriority=function(prio){this._setChildNode("priority",prio);return this;};JSJaCPresence.prototype.setPresence=function(show,status,prio){if(show)
this.setShow(show);if(status)
this.setStatus(status);if(prio)
this.setPriority(prio);return this;};JSJaCPresence.prototype.getStatus=function(){return this.getChildVal('status');};JSJaCPresence.prototype.getShow=function(){return this.getChildVal('show');};JSJaCPresence.prototype.getPriority=function(){return this.getChildVal('priority');};function JSJaCIQ(){this.base=JSJaCPacket;this.base('iq');}
JSJaCIQ.prototype=new JSJaCPacket;JSJaCIQ.prototype.setIQ=function(to,type,id){if(to)
this.setTo(to);if(type)
this.setType(type);if(id)
this.setID(id);return this;};JSJaCIQ.prototype.setQuery=function(xmlns){var query;try{query=this.getDoc().createElementNS(xmlns,'query');}catch(e){query=this.getDoc().createElement('query');query.setAttribute('xmlns',xmlns);}
this.getNode().appendChild(query);return query;};JSJaCIQ.prototype.getQuery=function(){return this.getNode().getElementsByTagName('query').item(0);};JSJaCIQ.prototype.getQueryXMLNS=function(){if(this.getQuery()){return this.getQuery().namespaceURI||this.getQuery().getAttribute('xmlns');}else{return null;}};JSJaCIQ.prototype.reply=function(payload){var rIQ=this.clone();rIQ.setTo(this.getFrom());rIQ.setFrom();rIQ.setType('result');if(payload){if(typeof payload=='string')
rIQ.getChild().appendChild(rIQ.getDoc().loadXML(payload));else if(payload.constructor==Array){var node=rIQ.getChild();for(var i=0;i<payload.length;i++)
if(typeof payload[i]=='string')
node.appendChild(rIQ.getDoc().loadXML(payload[i]));else if(typeof payload[i]=='object')
node.appendChild(payload[i]);}
else if(typeof payload=='object')
rIQ.getChild().appendChild(payload);}
return rIQ;};function JSJaCMessage(){this.base=JSJaCPacket;this.base('message');}
JSJaCMessage.prototype=new JSJaCPacket;JSJaCMessage.prototype.setBody=function(body){this._setChildNode("body",body);return this;};JSJaCMessage.prototype.setSubject=function(subject){this._setChildNode("subject",subject);return this;};JSJaCMessage.prototype.setThread=function(thread){this._setChildNode("thread",thread);return this;};JSJaCMessage.prototype.getThread=function(){return this.getChildVal('thread');};JSJaCMessage.prototype.getBody=function(){return this.getChildVal('body');};JSJaCMessage.prototype.getSubject=function(){return this.getChildVal('subject');};JSJaCPacket.wrapNode=function(node){var oPacket=null;switch(node.nodeName.toLowerCase()){case'presence':oPacket=new JSJaCPresence();break;case'message':oPacket=new JSJaCMessage();break;case'iq':oPacket=new JSJaCIQ();break;}
if(oPacket){oPacket.getDoc().replaceChild(oPacket._importNode(node,true),oPacket.getNode());}
return oPacket;};function JSJaCConnection(oArg){if(oArg&&oArg.httpbase)
this._httpbase=oArg.httpbase;if(oArg&&oArg.oDbg&&oArg.oDbg.log){this.oDbg=oArg.oDbg;}else{this.oDbg={log:function(){}};}
if(oArg&&oArg.timerval)
this.setPollInterval(oArg.timerval);else
this.setPollInterval(JSJAC_TIMERVAL);if(oArg&&oArg.cookie_prefix)
this._cookie_prefix=oArg.cookie_prefix;else
this._cookie_prefix="";this._connected=false;this._events=[];this._keys=null;this._ID=0;this._inQ=[];this._pQueue=[];this._regIDs=[];this._req=[];this._status='intialized';this._errcnt=0;this._inactivity=JSJAC_INACTIVITY;this._sendRawCallbacks=[];}
JSJaCConnection.prototype.connect=function(oArg){this._setStatus('connecting');if(oArg.authtype!='x-facebook-platform'){this.domain=oArg.domain||'localhost';this.username=oArg.username;this.resource=oArg.resource;this.pass=oArg.password||oArg.pass;this.register=oArg.register;this.authhost=oArg.authhost||oArg.host||oArg.domain;this.authtype=oArg.authtype||'sasl';}else{this.domain='chat.facebook.com';this.authtype=oArg.authtype;if(oArg.facebookApp!==undefined){this._facebookApp=oArg.facebookApp;if(!document.getElementById('fb-root')){var fbDiv=document.createElement('div');fbDiv.id='fb-root';document.body.appendChild(fbDiv);}
if(oArg.facebookApp.getSession()===undefined){this._facebookApp.Login(this,oArg);return;}}else{this.oDbg.log("No Facebook application param specified!",1);return;}}
if(oArg.xmllang&&oArg.xmllang!=='')
this._xmllang=oArg.xmllang;else
this._xmllang='en';if(oArg.allow_plain)
this._allow_plain=oArg.allow_plain;else
this._allow_plain=JSJAC_ALLOW_PLAIN;this.host=oArg.host;this.port=oArg.port||5222;if(oArg.secure)
this.secure='true';else
this.secure='false';this.jid=this.username+'@'+this.domain;this.fulljid=this.jid+'/'+this.resource;this._rid=Math.round(100000.5+(((900000.49999)-(100000.5))*Math.random()));var slot=this._getFreeSlot();this._req[slot]=this._setupRequest(true);var reqstr=this._getInitialRequestString();this.oDbg.log(reqstr,4);this._req[slot].r.onreadystatechange=JSJaC.bind(function(){var r=this._req[slot].r;if(r.readyState==4){this.oDbg.log("async recv: "+r.responseText,4);this._handleInitialResponse(r);}},this);if(typeof(this._req[slot].r.onerror)!='undefined'){this._req[slot].r.onerror=JSJaC.bind(function(e){this.oDbg.log('XmlHttpRequest error',1);},this);}
this._req[slot].r.send(reqstr);};JSJaCConnection.prototype.connected=function(){return this._connected;};JSJaCConnection.prototype.disconnect=function(){this._setStatus('disconnecting');if(!this.connected())
return;this._connected=false;clearInterval(this._interval);clearInterval(this._inQto);if(this._timeout)
clearTimeout(this._timeout);var slot=this._getFreeSlot();this._req[slot]=this._setupRequest(false);var request=this._getRequestString(false,true);this.oDbg.log("Disconnecting: "+request,4);try{this._req[slot].r.send(request);}catch(e){}
this.oDbg.log("disconnected");try{JSJaCCookie.read(this._cookie_prefix+'JSJaC_State').erase();}catch(e){}
this._handleEvent('ondisconnect');};JSJaCConnection.prototype.getPollInterval=function(){return this._timerval;};JSJaCConnection.prototype.registerHandler=function(event){event=event.toLowerCase();var eArg={handler:arguments[arguments.length-1],childName:'*',childNS:'*',type:'*'};if(arguments.length>2)
eArg.childName=arguments[1];if(arguments.length>3)
eArg.childNS=arguments[2];if(arguments.length>4)
eArg.type=arguments[3];if(!this._events[event])
this._events[event]=new Array(eArg);else
this._events[event]=this._events[event].concat(eArg);this._events[event]=this._events[event].sort(function(a,b){var aRank=0;var bRank=0;with(a){if(type=='*')
aRank++;if(childNS=='*')
aRank++;if(childName=='*')
aRank++;}
with(b){if(type=='*')
bRank++;if(childNS=='*')
bRank++;if(childName=='*')
bRank++;}
if(aRank>bRank)
return 1;if(aRank<bRank)
return-1;return 0;});this.oDbg.log("registered handler for event '"+event+"'",2);};JSJaCConnection.prototype.unregisterHandler=function(event,handler){event=event.toLowerCase();if(!this._events[event])
return;var arr=this._events[event],res=[];for(var i=0;i<arr.length;i++)
if(arr[i].handler!=handler)
res.push(arr[i]);if(arr.length!=res.length){this._events[event]=res;this.oDbg.log("unregistered handler for event '"+event+"'",2);}};JSJaCConnection.prototype.registerIQGet=function(childName,childNS,handler){this.registerHandler('iq',childName,childNS,'get',handler);};JSJaCConnection.prototype.registerIQSet=function(childName,childNS,handler){this.registerHandler('iq',childName,childNS,'set',handler);};JSJaCConnection.prototype.resume=function(){try{var json=JSJaCCookie.read(this._cookie_prefix+'JSJaC_State').getValue();this.oDbg.log('read cookie: '+json,2);JSJaCCookie.read(this._cookie_prefix+'JSJaC_State').erase();return this.resumeFromData(JSJaCJSON.parse(json));}catch(e){}
return false;};JSJaCConnection.prototype.resumeFromData=function(data){try{this._setStatus('resuming');for(var i in data)
if(data.hasOwnProperty(i))
this[i]=data[i];if(this._keys){this._keys2=new JSJaCKeys();var u=this._keys2._getSuspendVars();for(var j=0;j<u.length;j++)
this._keys2[u[j]]=this._keys[u[j]];this._keys=this._keys2;}
if(this._connected){this._handleEvent('onresume');setTimeout(JSJaC.bind(this._resume,this),this.getPollInterval());this._interval=setInterval(JSJaC.bind(this._checkQueue,this),JSJAC_CHECKQUEUEINTERVAL);this._inQto=setInterval(JSJaC.bind(this._checkInQ,this),JSJAC_CHECKINQUEUEINTERVAL);}
return(this._connected===true);}catch(e){if(e.message)
this.oDbg.log("Resume failed: "+e.message,1);else
this.oDbg.log("Resume failed: "+e,1);return false;}};JSJaCConnection.prototype.send=function(packet,cb,arg){if(!packet||!packet.pType){this.oDbg.log("no packet: "+packet,1);return false;}
if(!this.connected())
return false;if(cb){if(!packet.getID())
packet.setID('JSJaCID_'+this._ID++);this._registerPID(packet.getID(),cb,arg);}
this._pQueue=this._pQueue.concat(packet.xml());this._handleEvent(packet.pType()+'_out',packet);this._handleEvent("packet_out",packet);return true;};JSJaCConnection.prototype.sendIQ=function(iq,handlers,arg){if(!iq||iq.pType()!='iq'){return false;}
handlers=handlers||{};var error_handler=handlers.error_handler||JSJaC.bind(function(aIq){this.oDbg.log(aIq.xml(),1);},this);var result_handler=handlers.result_handler||JSJaC.bind(function(aIq){this.oDbg.log(aIq.xml(),2);},this);var iqHandler=function(aIq,arg){switch(aIq.getType()){case'error':error_handler(aIq);break;case'result':result_handler(aIq,arg);break;}};return this.send(iq,iqHandler,arg);};JSJaCConnection.prototype.setPollInterval=function(timerval){if(timerval&&!isNaN(timerval))
this._timerval=timerval;return this._timerval;};JSJaCConnection.prototype.status=function(){return this._status;};JSJaCConnection.prototype.suspend=function(){var data=this.suspendToData();try{var c=new JSJaCCookie(this._cookie_prefix+'JSJaC_State',JSJaCJSON.toString(data));this.oDbg.log("writing cookie: "+c.getValue()+"\n"+"(length:"+c.getValue().length+")",2);c.write();var c2=JSJaCCookie.get(this._cookie_prefix+'JSJaC_State');if(c.getValue()!=c2){this.oDbg.log("Suspend failed writing cookie.\nread: "+c2,1);c.erase();return false;}
return true;}catch(e){this.oDbg.log("Failed creating cookie '"+this._cookie_prefix+"JSJaC_State': "+e.message,1);}
return false;};JSJaCConnection.prototype.suspendToData=function(){clearTimeout(this._timeout);clearInterval(this._interval);clearInterval(this._inQto);this._suspend();var u=('_connected,_keys,_ID,_xmllang,_inQ,_pQueue,_regIDs,_errcnt,_inactivity,domain,username,resource,jid,fulljid,_sid,_httpbase,_timerval,_is_polling').split(',');u=u.concat(this._getSuspendVars());var s={};for(var i=0;i<u.length;i++){if(!this[u[i]])continue;var o={};if(this[u[i]]._getSuspendVars){var uo=this[u[i]]._getSuspendVars();for(var j=0;j<uo.length;j++)
o[uo[j]]=this[u[i]][uo[j]];}else
o=this[u[i]];s[u[i]]=o;}
this._connected=false;this._setStatus('suspending');return s;};JSJaCConnection.prototype._abort=function(){clearTimeout(this._timeout);clearInterval(this._inQto);clearInterval(this._interval);this._connected=false;this._setStatus('aborted');this.oDbg.log("Disconnected.",1);this._handleEvent('ondisconnect');this._handleEvent('onerror',JSJaCError('500','cancel','service-unavailable'));};JSJaCConnection.prototype._checkInQ=function(){for(var i=0;i<this._inQ.length&&i<10;i++){var item=this._inQ[0];this._inQ=this._inQ.slice(1,this._inQ.length);var packet=JSJaCPacket.wrapNode(item);if(!packet)
return;this._handleEvent("packet_in",packet);if(packet.pType&&!this._handlePID(packet)){this._handleEvent(packet.pType()+'_in',packet);this._handleEvent(packet.pType(),packet);}}};JSJaCConnection.prototype._checkQueue=function(){if(this._pQueue.length>0)
this._process();return true;};JSJaCConnection.prototype._doAuth=function(){if(this.has_sasl&&this.authtype=='nonsasl')
this.oDbg.log("Warning: SASL present but not used",1);if(!this._doSASLAuth()&&!this._doLegacyAuth()){this.oDbg.log("Auth failed for authtype "+this.authtype,1);this.disconnect();return false;}
return true;};JSJaCConnection.prototype._doInBandReg=function(){if(this.authtype=='saslanon'||this.authtype=='anonymous')
return;var iq=new JSJaCIQ();iq.setType('set');iq.setID('reg1');iq.appendNode("query",{xmlns:NS_REGISTER},[["username",this.username],["password",this.pass]]);this.send(iq,this._doInBandRegDone);};JSJaCConnection.prototype._doInBandRegDone=function(iq){if(iq&&iq.getType()=='error'){this.oDbg.log("registration failed for "+this.username,0);this._handleEvent('onerror',iq.getChild('error'));return;}
this.oDbg.log(this.username+" registered succesfully",0);this._doAuth();};JSJaCConnection.prototype._doLegacyAuth=function(){if(this.authtype!='nonsasl'&&this.authtype!='anonymous')
return false;var iq=new JSJaCIQ();iq.setIQ(null,'get','auth1');iq.appendNode('query',{xmlns:NS_AUTH},[['username',this.username]]);this.send(iq,this._doLegacyAuth2);return true;};JSJaCConnection.prototype._doLegacyAuth2=function(resIq){if(!resIq||resIq.getType()!='result'){if(resIq&&resIq.getType()=='error')
this._handleEvent('onerror',resIq.getChild('error'));this.disconnect();return;}
var use_digest=(resIq.getChild('digest')!==null);var iq=new JSJaCIQ();iq.setIQ(null,'set','auth2');var query=iq.appendNode('query',{xmlns:NS_AUTH},[['username',this.username],['resource',this.resource]]);if(use_digest){query.appendChild(iq.buildNode('digest',{xmlns:NS_AUTH},hex_sha1(this.streamid+this.pass)));}else if(this._allow_plain){query.appendChild(iq.buildNode('password',{xmlns:NS_AUTH},this.pass));}else{this.oDbg.log("no valid login mechanism found",1);this.disconnect();return;}
this.send(iq,this._doLegacyAuthDone);};JSJaCConnection.prototype._doLegacyAuthDone=function(iq){if(iq.getType()!='result'){if(iq.getType()=='error')
this._handleEvent('onerror',iq.getChild('error'));this.disconnect();}else
this._handleEvent('onconnect');};JSJaCConnection.prototype._doSASLAuth=function(){if(this.authtype=='nonsasl'||this.authtype=='anonymous')
return false;if(this.authtype=='saslanon'){if(this.mechs['ANONYMOUS']){this.oDbg.log("SASL using mechanism 'ANONYMOUS'",2);return this._sendRaw("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='ANONYMOUS'/>",this._doSASLAuthDone);}
this.oDbg.log("SASL ANONYMOUS requested but not supported",1);}else if(this.authtype=='x-facebook-platform'){if(this.mechs['X-FACEBOOK-PLATFORM']){return this._sendRaw("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='X-FACEBOOK-PLATFORM' />",this._doFacebookAuth);}
this.oDbg.log("X-FACEBOOK-PLATFORM requested but not supported",1);}else{if(this.mechs['DIGEST-MD5']){this.oDbg.log("SASL using mechanism 'DIGEST-MD5'",2);return this._sendRaw("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='DIGEST-MD5'/>",this._doSASLAuthDigestMd5S1);}else if(this._allow_plain&&this.mechs['PLAIN']){this.oDbg.log("SASL using mechanism 'PLAIN'",2);var authStr=this.username+'@'+
this.domain+String.fromCharCode(0)+
this.username+String.fromCharCode(0)+
this.pass;this.oDbg.log("authenticating with '"+authStr+"'",2);authStr=b64encode(authStr);return this._sendRaw("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN'>"+authStr+"</auth>",this._doSASLAuthDone);}
this.oDbg.log("No SASL mechanism applied",1);this.authtype='nonsasl';}
return false;};JSJaCConnection.prototype._doSASLAuthDigestMd5S1=function(el){if(el.nodeName!="challenge"){this.oDbg.log("challenge missing",1);this._handleEvent('onerror',JSJaCError('401','auth','not-authorized'));this.disconnect();}else{var challenge=b64decode(el.firstChild.nodeValue);this.oDbg.log("got challenge: "+challenge,2);this._nonce=challenge.substring(challenge.indexOf("nonce=")+7);this._nonce=this._nonce.substring(0,this._nonce.indexOf("\""));this.oDbg.log("nonce: "+this._nonce,2);if(this._nonce===''||this._nonce.indexOf('\"')!=-1){this.oDbg.log("nonce not valid, aborting",1);this.disconnect();return;}
this._digest_uri="xmpp/";this._digest_uri+=this.domain;this._cnonce=cnonce(14);this._nc='00000001';var A1=str_md5(this.username+':'+this.domain+':'+this.pass)+':'+this._nonce+':'+this._cnonce;var A2='AUTHENTICATE:'+this._digest_uri;var response=hex_md5(hex_md5(A1)+':'+this._nonce+':'+this._nc+':'+
this._cnonce+':auth:'+hex_md5(A2));var rPlain='username="'+this.username+'",realm="'+this.domain+'",nonce="'+this._nonce+'",cnonce="'+this._cnonce+'",nc="'+this._nc+'",qop=auth,digest-uri="'+this._digest_uri+'",response="'+response+'",charset="utf-8"';this.oDbg.log("response: "+rPlain,2);this._sendRaw("<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"+
binb2b64(str2binb(rPlain))+"</response>",this._doSASLAuthDigestMd5S2);}};JSJaCConnection.prototype._doSASLAuthDigestMd5S2=function(el){if(el.nodeName=='failure'){if(el.xml)
this.oDbg.log("auth error: "+el.xml,1);else
this.oDbg.log("auth error",1);this._handleEvent('onerror',JSJaCError('401','auth','not-authorized'));this.disconnect();return;}
var response=b64decode(el.firstChild.nodeValue);this.oDbg.log("response: "+response,2);var rspauth=response.substring(response.indexOf("rspauth=")+8);this.oDbg.log("rspauth: "+rspauth,2);var A1=str_md5(this.username+':'+this.domain+':'+this.pass)+':'+this._nonce+':'+this._cnonce;var A2=':'+this._digest_uri;var rsptest=hex_md5(hex_md5(A1)+':'+this._nonce+':'+this._nc+':'+
this._cnonce+':auth:'+hex_md5(A2));this.oDbg.log("rsptest: "+rsptest,2);if(rsptest!=rspauth){this.oDbg.log("SASL Digest-MD5: server repsonse with wrong rspauth",1);this.disconnect();return;}
if(el.nodeName=='success'){this._reInitStream(JSJaC.bind(this._doStreamBind,this));}else{this._sendRaw("<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>",this._doSASLAuthDone);}};JSJaCConnection.prototype._doSASLAuthDone=function(el){if(el.nodeName!='success'){this.oDbg.log("auth failed",1);this._handleEvent('onerror',JSJaCError('401','auth','not-authorized'));this.disconnect();}else{this._reInitStream(JSJaC.bind(this._doStreamBind,this));}};JSJaCConnection.prototype._doFacebookAuth=function(el){if(el.nodeName!="challenge"){this.oDbg.log("challenge missing",1);this._handleEvent('onerror',JSJaCError('401','auth','not-authorized'));this.disconnect();}else{var challenge=b64decode(el.firstChild.nodeValue);this.oDbg.log("got challenge: "+challenge,2);var parts=challenge.split('&');var vars=Array();for(var i=0;i<parts.length;i++){var tmp=parts[i].split('=');vars[tmp[0]]=tmp[1];}
if(vars['nonce']!=''){var fbSession=this._facebookApp.getSession();var response={'api_key':this._facebookApp.getApiKey(),'call_id':new Date().getTime(),'method':vars['method'],'nonce':vars['nonce'],'session_key':fbSession['session_key'],'v':'1.0'};response['sig']='api_key='+response['api_key']+'call_id='+response['call_id']+'method='+response['method']+'nonce='+response['nonce']+'session_key='+response['session_key']+'v='+response['v'];response['sig']=hex_md5(response['sig']+this._facebookApp.getApiSecret());response='api_key='+response['api_key']+'&'+'call_id='+response['call_id']+'&'+'method='+response['method']+'&'+'nonce='+response['nonce']+'&'+'session_key='+response['session_key']+'&'+'v='+response['v']+'&'+'sig='+response['sig'];response=b64encode(response);return this._sendRaw("<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"+response+"</response>",this._doFacebookAuthDone);}else{this.oDbg.log("nonce missing",1);this._handleEvent('onerror',JSJaCError('401','auth','not-authorized'));this.disconnect();}}};JSJaCConnection.prototype._doFacebookAuthDone=function(el){if(el.nodeName!='success'){this.oDbg.log("auth failed",1);this._handleEvent('onerror',JSJaCError('401','auth','not-authorized'));this.disconnect();}else{this._reInitStream(JSJaC.bind(this._doStreamBind,this));}};JSJaCConnection.prototype._doStreamBind=function(){var iq=new JSJaCIQ();iq.setIQ(null,'set','bind_1');iq.appendNode("bind",{xmlns:NS_BIND},[["resource",this.resource]]);this.oDbg.log(iq.xml());this.send(iq,this._doXMPPSess);};JSJaCConnection.prototype._doXMPPSess=function(iq){if(iq.getType()!='result'||iq.getType()=='error'){this.disconnect();if(iq.getType()=='error')
this._handleEvent('onerror',iq.getChild('error'));return;}
this.fulljid=iq.getChildVal("jid");this.jid=this.fulljid.substring(0,this.fulljid.lastIndexOf('/'));iq=new JSJaCIQ();iq.setIQ(null,'set','sess_1');iq.appendNode("session",{xmlns:NS_SESSION},[]);this.oDbg.log(iq.xml());this.send(iq,this._doXMPPSessDone);};JSJaCConnection.prototype._doXMPPSessDone=function(iq){if(iq.getType()!='result'||iq.getType()=='error'){this.disconnect();if(iq.getType()=='error')
this._handleEvent('onerror',iq.getChild('error'));return;}else
this._handleEvent('onconnect');};JSJaCConnection.prototype._handleEvent=function(event,arg){event=event.toLowerCase();this.oDbg.log("incoming event '"+event+"'",3);if(!this._events[event])
return;this.oDbg.log("handling event '"+event+"'",2);for(var i=0;i<this._events[event].length;i++){var aEvent=this._events[event][i];if(typeof aEvent.handler=='function'){try{if(arg){if(arg.pType){if((!arg.getNode().hasChildNodes()&&aEvent.childName!='*')||(arg.getNode().hasChildNodes()&&!arg.getChild(aEvent.childName,aEvent.childNS)))
continue;if(aEvent.type!='*'&&arg.getType()!=aEvent.type)
continue;this.oDbg.log(aEvent.childName+"/"+aEvent.childNS+"/"+aEvent.type+" => match for handler "+aEvent.handler,3);}
if(aEvent.handler(arg)){break;}}
else
if(aEvent.handler()){break;}}catch(e){if(e.fileName&&e.lineNumber){this.oDbg.log(aEvent.handler+"\n>>>"+e.name+": "+e.message+' in '+e.fileName+' line '+e.lineNumber,1);}else{this.oDbg.log(aEvent.handler+"\n>>>"+e.name+": "+e.message,1);}}}}};JSJaCConnection.prototype._handlePID=function(aJSJaCPacket){if(!aJSJaCPacket.getID())
return false;for(var i in this._regIDs){if(this._regIDs.hasOwnProperty(i)&&this._regIDs[i]&&i==aJSJaCPacket.getID()){var pID=aJSJaCPacket.getID();this.oDbg.log("handling "+pID,3);if(this._regIDs[i].cb.call(this,aJSJaCPacket,this._regIDs[i].arg)===false){return false;}else{this._unregisterPID(pID);return true;}}}
return false;};JSJaCConnection.prototype._handleResponse=function(req){var rootEl=this._parseResponse(req);if(!rootEl)
return;for(var i=0;i<rootEl.childNodes.length;i++){if(this._sendRawCallbacks.length){var cb=this._sendRawCallbacks[0];this._sendRawCallbacks=this._sendRawCallbacks.slice(1,this._sendRawCallbacks.length);cb.fn.call(this,rootEl.childNodes.item(i),cb.arg);continue;}
this._inQ=this._inQ.concat(rootEl.childNodes.item(i));}};JSJaCConnection.prototype._parseStreamFeatures=function(doc){if(!doc){this.oDbg.log("nothing to parse ... aborting",1);return false;}
var errorTag;if(doc.getElementsByTagNameNS){errorTag=doc.getElementsByTagNameNS(NS_STREAM,"error").item(0);}else{var errors=doc.getElementsByTagName("error");for(var i=0;i<errors.length;i++)
if(errors.item(i).namespaceURI==NS_STREAM||errors.item(i).getAttribute('xmlns')==NS_STREAM){errorTag=errors.item(i);break;}}
if(errorTag){this._setStatus("internal_server_error");clearTimeout(this._timeout);clearInterval(this._interval);clearInterval(this._inQto);this._handleEvent('onerror',JSJaCError('503','cancel','session-terminate'));this._connected=false;this.oDbg.log("Disconnected.",1);this._handleEvent('ondisconnect');return false;}
this.mechs={};var lMec1=doc.getElementsByTagName("mechanisms");if(!lMec1.length)return false;this.has_sasl=false;for(var i=0;i<lMec1.length;i++)
if(lMec1.item(i).getAttribute("xmlns")==NS_SASL){this.has_sasl=true;var lMec2=lMec1.item(i).getElementsByTagName("mechanism");for(var j=0;j<lMec2.length;j++)
this.mechs[lMec2.item(j).firstChild.nodeValue]=true;break;}
if(this.has_sasl)
this.oDbg.log("SASL detected",2);else{this.oDbg.log("No support for SASL detected",2);return true;}
return true;};JSJaCConnection.prototype._process=function(timerval){if(!this.connected()){this.oDbg.log("Connection lost ...",1);if(this._interval)
clearInterval(this._interval);return;}
this.setPollInterval(timerval);if(this._timeout)
clearTimeout(this._timeout);var slot=this._getFreeSlot();if(slot<0)
return;if(typeof(this._req[slot])!='undefined'&&typeof(this._req[slot].r)!='undefined'&&this._req[slot].r.readyState!=4){this.oDbg.log("Slot "+slot+" is not ready");return;}
if(!this.isPolling()&&this._pQueue.length===0&&this._req[(slot+1)%2]&&this._req[(slot+1)%2].r.readyState!=4){this.oDbg.log("all slots busy, standby ...",2);return;}
if(!this.isPolling())
this.oDbg.log("Found working slot at "+slot,2);this._req[slot]=this._setupRequest(true);this._req[slot].r.onreadystatechange=JSJaC.bind(function(){if(!this.connected())
return;if(this._req[slot].r.readyState==4){this._setStatus('processing');this.oDbg.log("async recv: "+this._req[slot].r.responseText,4);this._handleResponse(this._req[slot]);if(this._pQueue.length){this._timeout=setTimeout(JSJaC.bind(this._process,this),100);}else{this.oDbg.log("scheduling next poll in "+this.getPollInterval()+" msec",4);this._timeout=setTimeout(JSJaC.bind(this._process,this),this.getPollInterval());}}},this);try{this._req[slot].r.onerror=JSJaC.bind(function(){if(!this.connected())
return;this._errcnt++;this.oDbg.log('XmlHttpRequest error ('+this._errcnt+')',1);if(this._errcnt>JSJAC_ERR_COUNT){this._abort();return;}
this._setStatus('onerror_fallback');setTimeout(JSJaC.bind(this._resume,this),JSJAC_RETRYDELAY);return;},this);}catch(e){}
var reqstr=this._getRequestString();if(typeof(this._rid)!='undefined')
this._req[slot].rid=this._rid;this.oDbg.log("sending: "+reqstr,4);this._req[slot].r.send(reqstr);};JSJaCConnection.prototype._registerPID=function(pID,cb,arg){if(!pID||!cb)
return false;this._regIDs[pID]={};this._regIDs[pID].cb=cb;if(arg)
this._regIDs[pID].arg=arg;this.oDbg.log("registered "+pID,3);return true;};JSJaCConnection.prototype._prepSendEmpty=function(cb,ctx){return function(){ctx._sendEmpty(JSJaC.bind(cb,ctx));};};JSJaCConnection.prototype._sendEmpty=function(cb){var slot=this._getFreeSlot();this._req[slot]=this._setupRequest(true);this._req[slot].r.onreadystatechange=JSJaC.bind(function(){if(this._req[slot].r.readyState==4){this.oDbg.log("async recv: "+this._req[slot].r.responseText,4);cb(this._req[slot].r);}},this);if(typeof(this._req[slot].r.onerror)!='undefined'){this._req[slot].r.onerror=JSJaC.bind(function(e){this.oDbg.log('XmlHttpRequest error',1);},this);}
var reqstr=this._getRequestString();this.oDbg.log("sending: "+reqstr,4);this._req[slot].r.send(reqstr);};JSJaCConnection.prototype._sendRaw=function(xml,cb,arg){if(cb)
this._sendRawCallbacks.push({fn:cb,arg:arg});this._pQueue.push(xml);this._process();return true;};JSJaCConnection.prototype._setStatus=function(status){if(!status||status==='')
return;if(status!=this._status){this._status=status;this._handleEvent('onstatuschanged',status);this._handleEvent('status_changed',status);}};JSJaCConnection.prototype._unregisterPID=function(pID){if(!this._regIDs[pID])
return false;this._regIDs[pID]=null;this.oDbg.log("unregistered "+pID,3);return true;};function JSJaCHttpBindingConnection(oArg){this.base=JSJaCConnection;this.base(oArg);this._hold=JSJACHBC_MAX_HOLD;this._inactivity=0;this._last_requests={};this._last_rid=0;this._min_polling=0;this._pause=0;this._wait=oArg.wait||JSJACHBC_MAX_WAIT;}
JSJaCHttpBindingConnection.prototype=new JSJaCConnection();JSJaCHttpBindingConnection.prototype.inherit=function(oArg){if(oArg.jid){var oJid=new JSJaCJID(oArg.jid);this.domain=oJid.getDomain();this.username=oJid.getNode();this.resource=oJid.getResource();}else{this.domain=oArg.domain||'localhost';this.username=oArg.username;this.resource=oArg.resource;}
this._sid=oArg.sid;this._rid=oArg.rid;this._min_polling=oArg.polling;this._inactivity=oArg.inactivity;this._setHold(oArg.requests-1);this.setPollInterval(this._timerval);if(oArg.wait)
this._wait=oArg.wait;this._connected=true;this._handleEvent('onconnect');this._interval=setInterval(JSJaC.bind(this._checkQueue,this),JSJAC_CHECKQUEUEINTERVAL);this._inQto=setInterval(JSJaC.bind(this._checkInQ,this),JSJAC_CHECKINQUEUEINTERVAL);this._timeout=setTimeout(JSJaC.bind(this._process,this),this.getPollInterval());};JSJaCHttpBindingConnection.prototype.setPollInterval=function(timerval){if(timerval&&!isNaN(timerval)){if(!this.isPolling())
this._timerval=100;else if(this._min_polling&&timerval<this._min_polling*1000)
this._timerval=this._min_polling*1000;else if(this._inactivity&&timerval>this._inactivity*1000)
this._timerval=this._inactivity*1000;else
this._timerval=timerval;}
return this._timerval;};JSJaCHttpBindingConnection.prototype.isPolling=function(){return(this._hold===0);};JSJaCHttpBindingConnection.prototype._getFreeSlot=function(){for(var i=0;i<this._hold+1;i++)
if(typeof(this._req[i])=='undefined'||typeof(this._req[i].r)=='undefined'||this._req[i].r.readyState==4)
return i;return-1;};JSJaCHttpBindingConnection.prototype._getHold=function(){return this._hold;};JSJaCHttpBindingConnection.prototype._getRequestString=function(raw,last){raw=raw||'';var reqstr='';if(this._rid<=this._last_rid&&typeof(this._last_requests[this._rid])!='undefined')
reqstr=this._last_requests[this._rid].xml;else{var xml='';while(this._pQueue.length){var curNode=this._pQueue[0];xml+=curNode;this._pQueue=this._pQueue.slice(1,this._pQueue.length);}
reqstr="<body rid='"+this._rid+"' sid='"+this._sid+"' xmlns='http://jabber.org/protocol/httpbind' ";if(JSJAC_HAVEKEYS){reqstr+="key='"+this._keys.getKey()+"' ";if(this._keys.lastKey()){this._keys=new JSJaCKeys(hex_sha1,this.oDbg);reqstr+="newkey='"+this._keys.getKey()+"' ";}}
if(last)
reqstr+="type='terminate'";else if(this._reinit){if(JSJACHBC_USE_BOSH_VER)
reqstr+="xml:lang='"+this._xmllang+"' xmpp:restart='true' xmlns:xmpp='urn:xmpp:xbosh' to='"+this.domain+"'";this._reinit=false;}
if(xml!=''||raw!=''){reqstr+=">"+raw+xml+"</body>";}else{reqstr+="> </body>";}
this._last_requests[this._rid]={};this._last_requests[this._rid].xml=reqstr;this._last_rid=this._rid;for(var i in this._last_requests)
if(this._last_requests.hasOwnProperty(i)&&i<this._rid-this._hold)
delete(this._last_requests[i]);}
return reqstr;};JSJaCHttpBindingConnection.prototype._getInitialRequestString=function(){var reqstr="<body content='text/xml; charset=utf-8' hold='"+this._hold+"' xmlns='http://jabber.org/protocol/httpbind' to='"+this.authhost+"' wait='"+this._wait+"' rid='"+this._rid+"'";if(this.host&&this.port)
reqstr+=" route='xmpp:"+this.host+":"+this.port+"'";if(this.secure)
reqstr+=" secure='"+this.secure+"'";if(JSJAC_HAVEKEYS){this._keys=new JSJaCKeys(hex_sha1,this.oDbg);var key=this._keys.getKey();reqstr+=" newkey='"+key+"'";}
reqstr+=" xml:lang='"+this._xmllang+"'";if(JSJACHBC_USE_BOSH_VER){reqstr+=" ver='"+JSJACHBC_BOSH_VERSION+"'";reqstr+=" xmlns:xmpp='urn:xmpp:xbosh'";if(this.authtype=='sasl'||this.authtype=='saslanon'||this.authtype=='x-facebook-platform')
reqstr+=" xmpp:version='1.0'";}
reqstr+="/>";return reqstr;};JSJaCHttpBindingConnection.prototype._getStreamID=function(req){this.oDbg.log(req.responseText,4);if(!req.responseXML||!req.responseXML.documentElement){this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));return;}
var body=req.responseXML.documentElement;if(body.getAttribute('type')=='terminate'){this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));return;}
if(body.getAttribute('authid')){this.streamid=body.getAttribute('authid');this.oDbg.log("got streamid: "+this.streamid,2);}
if(!this._parseStreamFeatures(body)){this._sendEmpty(JSJaC.bind(this._getStreamID,this));return;}
this._timeout=setTimeout(JSJaC.bind(this._process,this),this.getPollInterval());if(this.register)
this._doInBandReg();else
this._doAuth();};JSJaCHttpBindingConnection.prototype._getSuspendVars=function(){return('host,port,secure,_rid,_last_rid,_wait,_min_polling,_inactivity,_hold,_last_requests,_pause').split(',');};JSJaCHttpBindingConnection.prototype._handleInitialResponse=function(req){try{this.oDbg.log(req.getAllResponseHeaders(),4);this.oDbg.log(req.responseText,4);}catch(ex){this.oDbg.log("No response",4);}
if(req.status!=200||!req.responseXML){this.oDbg.log("initial response broken (status: "+req.status+")",1);this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));return;}
var body=req.responseXML.documentElement;if(!body||body.tagName!='body'||body.namespaceURI!=NS_BOSH){this.oDbg.log("no body element or incorrect body in initial response",1);this._handleEvent("onerror",JSJaCError("500","wait","internal-service-error"));return;}
if(body.getAttribute("type")=="terminate"){this.oDbg.log("invalid response:\n"+req.responseText,1);clearTimeout(this._timeout);this._connected=false;this.oDbg.log("Disconnected.",1);this._handleEvent('ondisconnect');this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));return;}
this._sid=body.getAttribute('sid');this.oDbg.log("got sid: "+this._sid,2);if(body.getAttribute('polling'))
this._min_polling=body.getAttribute('polling');if(body.getAttribute('inactivity'))
this._inactivity=body.getAttribute('inactivity');if(body.getAttribute('requests'))
this._setHold(body.getAttribute('requests')-1);this.oDbg.log("set hold to "+this._getHold(),2);if(body.getAttribute('ver'))
this._bosh_version=body.getAttribute('ver');if(body.getAttribute('maxpause'))
this._pause=Number.min(body.getAttribute('maxpause'),JSJACHBC_MAXPAUSE);this.setPollInterval(this._timerval);this._connected=true;this._inQto=setInterval(JSJaC.bind(this._checkInQ,this),JSJAC_CHECKINQUEUEINTERVAL);this._interval=setInterval(JSJaC.bind(this._checkQueue,this),JSJAC_CHECKQUEUEINTERVAL);this._getStreamID(req);};JSJaCHttpBindingConnection.prototype._parseResponse=function(req){if(!this.connected()||!req)
return null;var r=req.r;try{if(r.status==404||r.status==403){this._abort();return null;}
if(r.status!=200||!r.responseXML){this._errcnt++;var errmsg="invalid response ("+r.status+"):\n"+r.getAllResponseHeaders()+"\n"+r.responseText;if(!r.responseXML)
errmsg+="\nResponse failed to parse!";this.oDbg.log(errmsg,1);if(this._errcnt>JSJAC_ERR_COUNT){this._abort();return null;}
if(this.connected()){this.oDbg.log("repeating ("+this._errcnt+")",1);this._setStatus('proto_error_fallback');setTimeout(JSJaC.bind(this._resume,this),this.getPollInterval());}
return null;}}catch(e){this.oDbg.log("XMLHttpRequest error: status not available",1);this._errcnt++;if(this._errcnt>JSJAC_ERR_COUNT){this._abort();}else{if(this.connected()){this.oDbg.log("repeating ("+this._errcnt+")",1);this._setStatus('proto_error_fallback');setTimeout(JSJaC.bind(this._resume,this),this.getPollInterval());}}
return null;}
var body=r.responseXML.documentElement;if(!body||body.tagName!='body'||body.namespaceURI!=NS_BOSH){this.oDbg.log("invalid response:\n"+r.responseText,1);clearTimeout(this._timeout);clearInterval(this._interval);clearInterval(this._inQto);this._connected=false;this.oDbg.log("Disconnected.",1);this._handleEvent('ondisconnect');this._setStatus('internal_server_error');this._handleEvent('onerror',JSJaCError('500','wait','internal-server-error'));return null;}
if(typeof(req.rid)!='undefined'&&this._last_requests[req.rid]){if(this._last_requests[req.rid].handled){this.oDbg.log("already handled "+req.rid,2);return null;}else
this._last_requests[req.rid].handled=true;}
if(body.getAttribute("type")=="terminate"){var condition=body.getAttribute('condition');if(condition!="item-not-found"){this.oDbg.log("session terminated:\n"+r.responseText,1);clearTimeout(this._timeout);clearInterval(this._interval);clearInterval(this._inQto);try{JSJaCCookie.read(this._cookie_prefix+'JSJaC_State').erase();}catch(e){}
this._connected=false;condition=body.getAttribute('condition');if(condition=="remote-stream-error")
if(body.getElementsByTagName("conflict").length>0)
this._setStatus("session-terminate-conflict");if(condition==null)
condition='session-terminate';this._handleEvent('onerror',JSJaCError('503','cancel',condition));this.oDbg.log("Aborting remaining connections",4);for(var i=0;i<this._hold+1;i++){try{this._req[i].r.abort();}catch(e){this.oDbg.log(e,1);}}
this.oDbg.log("parseResponse done with terminating",3);this.oDbg.log("Disconnected.",1);this._handleEvent('ondisconnect');}else{this._errcnt++;if(this._errcnt>JSJAC_ERR_COUNT)
this._abort();}
return null;}
this._errcnt=0;return r.responseXML.documentElement;};JSJaCHttpBindingConnection.prototype._reInitStream=function(cb){this._reinit=true;this._sendEmpty(this._prepReInitStreamWait(cb));};JSJaCHttpBindingConnection.prototype._prepReInitStreamWait=function(cb){return JSJaC.bind(function(req){this._reInitStreamWait(req,cb);},this);};JSJaCHttpBindingConnection.prototype._reInitStreamWait=function(req,cb){this.oDbg.log("checking for stream features");var doc=req.responseXML.documentElement,features,bind;this.oDbg.log(doc);if(doc.getElementsByTagNameNS){this.oDbg.log("checking with namespace");features=doc.getElementsByTagNameNS(NS_STREAM,'features').item(0);if(features){bind=features.getElementsByTagNameNS(NS_BIND,'bind').item(0);}}else{var featuresNL=doc.getElementsByTagName('stream:features'),i,l;for(i=0,l=featuresNL.length;i<l;i++){if(featuresNL.item(i).namespaceURI==NS_STREAM||featuresNL.item(i).getAttribute('xmlns')==NS_STREAM){features=featuresNL.item(i);break;}}
if(features){bind=features.getElementsByTagName('bind');for(i=0,l=bind.length;i<l;i++){if(bind.item(i).namespaceURI==NS_BIND||bind.item(i).getAttribute('xmlns')==NS_BIND){bind=bind.item(i);break;}}}}
this.oDbg.log(features);this.oDbg.log(bind);if(features){if(bind){cb();}else{this.oDbg.log("no bind feature - giving up",1);this._handleEvent('onerror',JSJaCError('503','cancel',"service-unavailable"));this._connected=false;this.oDbg.log("Disconnected.",1);this._handleEvent('ondisconnect');}}else{this._sendEmpty(this._prepReInitStreamWait(cb));}};JSJaCHttpBindingConnection.prototype._resume=function(){if(this._pause==0&&this._rid>=this._last_rid)
this._rid=this._last_rid-1;this._process();};JSJaCHttpBindingConnection.prototype._setHold=function(hold){if(!hold||isNaN(hold)||hold<0)
hold=0;else if(hold>JSJACHBC_MAX_HOLD)
hold=JSJACHBC_MAX_HOLD;this._hold=hold;return this._hold;};JSJaCHttpBindingConnection.prototype._setupRequest=function(async){var req={};var r=XmlHttp.create();try{r.open("POST",this._httpbase,async);r.setRequestHeader('Content-Type','text/xml; charset=utf-8');}catch(e){this.oDbg.log(e,1);}
req.r=r;this._rid++;req.rid=this._rid;return req;};JSJaCHttpBindingConnection.prototype._suspend=function(){if(this._pause==0)
return;var slot=this._getFreeSlot();this._req[slot]=this._setupRequest(false);var reqstr="<body pause='"+this._pause+"' xmlns='http://jabber.org/protocol/httpbind' sid='"+this._sid+"' rid='"+this._rid+"'";if(JSJAC_HAVEKEYS){reqstr+=" key='"+this._keys.getKey()+"'";if(this._keys.lastKey()){this._keys=new JSJaCKeys(hex_sha1,this.oDbg);reqstr+=" newkey='"+this._keys.getKey()+"'";}}
reqstr+=">";while(this._pQueue.length){var curNode=this._pQueue[0];reqstr+=curNode;this._pQueue=this._pQueue.slice(1,this._pQueue.length);}
reqstr+="</body>";this.oDbg.log("Disconnecting: "+reqstr,4);this._req[slot].r.send(reqstr);};function JSJaCWebSocketConnection(oArg){this.base=JSJaCConnection;this.base(oArg);this._ws=null;this.registerHandler('onerror',JSJaC.bind(this._cleanupWebSocket,this));}
JSJaCWebSocketConnection.prototype=new JSJaCConnection();JSJaCWebSocketConnection.prototype._cleanupWebSocket=function(){if(this._ws!==null){this._ws.onclose=null;this._ws.onerror=null;this._ws.onopen=null;this._ws.onmessage=null;this._ws.close();this._ws=null;}};JSJaCWebSocketConnection.prototype.connect=function(oArg){this._setStatus('connecting');this.domain=oArg.domain||'localhost';this.username=oArg.username;this.resource=oArg.resource;this.pass=oArg.pass;this.register=oArg.register;this.authhost=oArg.authhost||this.domain;this.authtype=oArg.authtype||'sasl';this.jid=this.username+'@'+this.domain;this.fulljid=this.jid+'/'+this.resource;if(typeof WebSocket==='undefined'){this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));return;}
this._ws=new WebSocket(this._httpbase,'xmpp');this._ws.onclose=JSJaC.bind(this._onclose,this);this._ws.onerror=JSJaC.bind(this._onerror,this);this._ws.onopen=JSJaC.bind(this._onopen,this);};JSJaCWebSocketConnection.prototype._onopen=function(){var reqstr=this._getInitialRequestString();this.oDbg.log(reqstr,4);this._ws.onmessage=JSJaC.bind(this._handleOpenStream,this);this._ws.send(reqstr);};JSJaCWebSocketConnection.prototype._handleOpenStream=function(event){var open,stream;this.oDbg.log(event.data,4);open=event.data;open=open.substr(open.indexOf('<stream:stream'));if(open.substr(-2)!=='/>'&&open.substr(-16)!=='</stream:stream>'){open+='</stream:stream>';}
stream=this._parseXml(open);if(!stream){this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));return;}
this.streamid=stream.getAttribute('id');this.oDbg.log('got streamid: '+this.streamid,2);this._ws.onmessage=JSJaC.bind(this._handleInitialResponse,this);};JSJaCWebSocketConnection.prototype._handleInitialResponse=function(event){var doc=this._parseXml(event.data);if(!this._parseStreamFeatures(doc)){this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));return;}
this._connected=true;if(this.register){this._doInBandReg();}else{this._doAuth();}};JSJaCWebSocketConnection.prototype.disconnect=function(){this._setStatus('disconnecting');if(!this.connected()){return;}
this._connected=false;this.oDbg.log('Disconnecting',4);this._cleanupWebSocket();this.oDbg.log('Disconnected',2);this._handleEvent('ondisconnect');};JSJaCWebSocketConnection.prototype._onclose=function(){this.oDbg.log('websocket closed',2);if(this._status!=='disconnecting'){this._connected=false;this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));}};JSJaCWebSocketConnection.prototype._onerror=function(){this.oDbg.log('websocket error',1);this._connected=false;this._handleEvent('onerror',JSJaCError('503','cancel','service-unavailable'));};JSJaCWebSocketConnection.prototype._onmessage=function(event){var stanza,node,packet;stanza=event.data;this._setStatus('processing');if(!stanza||stanza===''){return;}
node=this._parseXml(stanza);if(node.namespaceURI===NS_STREAM&&node.localName==='error'){if(node.getElementsByTagNameNS(NS_STREAMS,'conflict').length>0){this._setStatus('session-terminate-conflict');}
this._connected=false;this._handleEvent('onerror',JSJaCError('503','cancel','remote-stream-error'));return;}
packet=JSJaCPacket.wrapNode(node);if(!packet){return;}
this.oDbg.log('async recv: '+event.data,4);this._handleEvent('packet_in',packet);if(packet.pType&&!this._handlePID(packet)){this._handleEvent(packet.pType()+'_in',packet);this._handleEvent(packet.pType(),packet);}};JSJaCWebSocketConnection.prototype._parseXml=function(s){var doc;this.oDbg.log('Parsing: '+s,4);try{doc=XmlDocument.create('stream',NS_STREAM);if(s.indexOf('<stream:stream')===-1){doc.loadXML("<stream:stream xmlns:stream='"+NS_STREAM+"' xmlns='jabber:client'>"+s+"</stream:stream>");return doc.documentElement.firstChild;}else{doc.loadXML(s);return doc.documentElement;}}catch(e){this.oDbg.log('Error: '+e);this._connected=false;this._handleEvent('onerror',JSJaCError('500','wait','internal-service-error'));}
return null;};JSJaCWebSocketConnection.prototype._getInitialRequestString=function(){var streamto,reqstr;streamto=this.domain;if(this.authhost){streamto=this.authhost;}
reqstr='<stream:stream to="'+streamto+'" xmlns="jabber:client" xmlns:stream="'+NS_STREAM+'"';if(this.authtype==='sasl'||this.authtype==='saslanon'){reqstr+=' version="1.0"';}
reqstr+='>';return reqstr;};JSJaCWebSocketConnection.prototype.send=function(packet,cb,arg){this._ws.onmessage=JSJaC.bind(this._onmessage,this);if(!packet||!packet.pType){this.oDbg.log('no packet: '+packet,1);return false;}
if(!this.connected()){return false;}
if(cb){if(!packet.getID()){packet.setID('JSJaCID_'+this._ID++);}
this._registerPID(packet.getID(),cb,arg);}
try{this._handleEvent(packet.pType()+'_out',packet);this._handleEvent('packet_out',packet);this._ws.send(packet.xml());}catch(e){this.oDbg.log(e.toString(),1);return false;}
return true;};JSJaCWebSocketConnection.prototype.resume=function(){return false;};JSJaCWebSocketConnection.prototype.suspend=function(){return false;};JSJaCWebSocketConnection.prototype._doSASLAuthDigestMd5S1=function(event){var el=this._parseXml(event.data);return JSJaC.bind(JSJaCConnection.prototype._doSASLAuthDigestMd5S1,this)(el);};JSJaCWebSocketConnection.prototype._doSASLAuthDigestMd5S2=function(event){var el=this._parseXml(event.data);return JSJaC.bind(JSJaCConnection.prototype._doSASLAuthDigestMd5S2,this)(el);};JSJaCWebSocketConnection.prototype._doSASLAuthDone=function(event){var el=this._parseXml(event.data);return JSJaC.bind(JSJaCConnection.prototype._doSASLAuthDone,this)(el);};JSJaCWebSocketConnection.prototype._reInitStream=function(cb){var reqstr,streamto=this.domain;if(this.authhost){streamto=this.authhost;}
reqstr='<stream:stream xmlns:stream="'+NS_STREAM+'" xmlns="jabber:client" to="'+streamto+'" version="1.0">';this._sendRaw(reqstr,cb);};JSJaCWebSocketConnection.prototype._sendRaw=function(xml,cb,arg){if(cb){this._ws.onmessage=JSJaC.bind(cb,this,arg);}
this._ws.send(xml);return true;};function JSJaCFBApplication(oArg){if(oArg&&oArg.appID)
this._appID=oArg.appID;if(oArg&&oArg.apiKey)
this._apiKey=oArg.apiKey;if(oArg&&oArg.apiSecret)
this._apiSecret=oArg.apiSecret;this._perms='';this._session=undefined;};JSJaCFBApplication.prototype.Login=function(conn,oArg){var me=this;FB.init({appId:this._appID,status:true});FB.login(function(response){if(response.session){if(response.perms){me._perms=response.perms;me._session=response.session;conn.connect(oArg);}}},{perms:'xmpp_login'});};JSJaCFBApplication.prototype.getAppID=function(){return this._appID;};JSJaCFBApplication.prototype.getApiKey=function(){return this._apiKey;};JSJaCFBApplication.prototype.getApiSecret=function(){return this._apiSecret;};JSJaCFBApplication.prototype.getSession=function(){return this._session;};var JSJaC={Version:'1.4',require:function(libraryName){document.write('<script type="text/javascript" src="'+libraryName+'"></script>');},load:function(){var includes=['xmlextras','jsextras','crypt','JSJaCConfig','JSJaCConstants','JSJaCCookie','JSJaCJSON','JSJaCJID','JSJaCBuilder','JSJaCPacket','JSJaCError','JSJaCKeys','JSJaCConnection','JSJaCHttpPollingConnection','JSJaCHttpBindingConnection','JSJaCConsoleLogger','JSJaCFBApplication','JSJaCWebSocketConnection'];var scripts=document.getElementsByTagName("script");var path='./',i;for(i=0;i<scripts.length;i++){if(scripts.item(i).src&&scripts.item(i).src.match(/JSJaC\.js$/)){path=scripts.item(i).src.replace(/JSJaC.js$/,'');break;}}
for(i=0;i<includes.length;i++)
this.require(path+includes[i]+'.js');},bind:function(fn,obj,optArg){return function(arg){return fn.apply(obj,[arg,optArg]);};}};if(typeof JSJaCConnection=='undefined')
JSJaC.load();