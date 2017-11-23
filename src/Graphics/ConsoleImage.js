"use strict";


// based on https://github.com/adriancooney/console.image
exports.consoleImage = function(scale) {
  return function(url) {
    function getBox(width, height) {
      return {
        string: "+",
        style: "font-size: 1px; padding: " + Math.floor(height/2) + "px " + Math.floor(width/2) + "px; line-height: " + height + "px;"
      }
    }
    scale = scale || 1;
    var img = new Image();

    img.onload = function() {
      var dim = getBox(this.width * scale, this.height * scale);
      console.log("%c" + dim.string, dim.style + "background: url(" + url + "); background-size: " + (this.width * scale) + "px " + (this.height * scale) + "px; color: transparent;");
    };

    img.src = url;
  }
};

exports.consoleSvgToPngImage = function(scale) {
  return function(svg) {
    var v = undefined;
    if (typeof window === "undefined") {
      v = require('viz.js');
    } else {
      v = Viz;
    }
    return v.svgXmlToPngBase64(svg, scale, function(err, data) {
      if (err) {console.error(err)}
      exports.consoleImage(scale, "data:image/png;base64,"+data);
    })
  }
}
