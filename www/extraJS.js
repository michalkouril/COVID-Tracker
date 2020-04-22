// Website with idea: https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
var isMobileBinding = new Shiny.InputBinding();
$.extend(isMobileBinding, {
  find: function(scope) {
    return $(scope).find(".mobile-element");
    callback();
  },
  getValue: function(el) {
    return /((iPhone)|(iPod)|(iPad)|(Android)|(BlackBerry))/.test(navigator.userAgent)
  },
  setValue: function(el, value) {
  },
  subscribe: function(el, callback) {
  },
  unsubscribe: function(el) {
  }
});

Shiny.inputBindings.register(isMobileBinding);

//User time
$(document).on('shiny:connected', function(event) {
  var now = new Date().toLocaleString('en-us', {timeZoneName:'short'});
  Shiny.setInputValue("clientTime", now);
  
  const Http = new XMLHttpRequest();
  const url='http://ip-api.com/json';
  
  Http.open("GET", url, true);
  Http.send();
  
  Http.onreadystatechange = (e) => {
    
    if(Http.responseText !== ""){
      console.log(Http.responseText);
      Shiny.setInputValue("ipLoc", Http.responseText);
    }
    
};
  
});

//$(document).ready(function(){
//  var header = $('.navbar> .container-fluid > .navbar-collapse');
//  header.append('<img src="headerLogo.png" align="right" height="40px">');

//});


  

