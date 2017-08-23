var _user$project$Native_HtmlUtil = function(localRuntime) {
  var canvas = (typeof document.createElement !== 'undefined') ? document.createElement('canvas') : null;
  var context = canvas ? canvas.getContext('2d') : null;

  function measureText(fontFamily, fontSize, s) {
    if (canvas) {
      context.font = fontSize + "px '" + fontFamily + "'";
      var metrics = context.measureText(s);
      return metrics.width;
    } else {
      return 0;
    }
  }
  return {
    measureText: F3(measureText)
  };
}();
