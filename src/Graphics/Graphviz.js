"use strict";



exports.viz_internal = function(data, format, engine, scale) {
  return Viz(data, {format: format, engine: engine, scale: scale})
}
