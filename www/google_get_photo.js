var url ='https://www.google.com/search?as_st=y&tbm=isch&hl=en-GB&as_q=Saint John's+Canada+City&as_epq=&as_oq=&as_eq=&cr=&as_sitesearch=&safe=images&tbs=isz:lt,islt:4mp';
   var page = new WebPage()
    var fs = require('fs');

var vWidth = 1080;
var vHeight = 1920; 

page.viewportSize = {
    width: vWidth ,
    height: vHeight 
};

//Scroll throu!
var s = 0;
var sBase = page.evaluate(function () { return document.body.scrollHeight; });
page.scrollPosition = {
    top: sBase,
    left: 0
};

function sc() {
    var sBase2 = page.evaluate(function () { return document.body.scrollHeight; });
    if (sBase2 != sBase) {
        sBase = sBase2;
    }
    if (s> sBase) {
        page.viewportSize = {width: vWidth, height: vHeight};
        return;
    }
    page.scrollPosition = {
        top: s,
        left: 0
    };
    page.viewportSize = {width: vWidth, height: s};
    s += Math.min(sBase/20,400);
    setTimeout(sc, 110);
}

function just_wait() {
    setTimeout(function() {
            fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}

page.open(url, function (status) {
	    sc();
        just_wait();
});
