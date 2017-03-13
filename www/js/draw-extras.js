
// Remove drawn features in leaflet map
LeafletWidget.methods.removeDrawnFeatures = function(targetGroup) {
  (function(){
    var map = this;
     map.layerManager.getLayerGroup(map._editableFeatureGroupName).clearLayers();
  }.call(this));
};
