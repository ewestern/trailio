
var getSegmentBoundsBySwByNe = function(sw, ne, srid, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/segment/bounds/' + encodeURIComponent(sw) + '/' + encodeURIComponent(ne) + '' + '?srid=' + encodeURIComponent(srid), true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

var getSegmentProximityByPointByDistance = function(point, distance, srid, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/segment/proximity/' + encodeURIComponent(point) + '/' + encodeURIComponent(distance) + '' + '?srid=' + encodeURIComponent(srid), true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};
