"use strict";



exports.viz_internal = function(data) {
  return function (format) {
    return function (engine) {
      return function (scale) {
        return Viz(data, {format: format, engine: engine, scale: scale})
      }
    }
  }
}
