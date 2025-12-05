// =====================================================================
// Sentinel-2 NDCI Analysis for Lower Deschutes River
// This script calculates the Normalized Difference Chlorophyll Index (NDCI)
// for two regions of interest (ROIs) along the Lower Deschutes River in Oregon,
// =====================================================================

var peltonTrapAOI = ee.Geometry.Polygon(
    // Pelton Trap AOI Coordinates:
    [[[-121.24978316834105, 44.7240001852607],
      [-121.24390376618041, 44.7240001852607],
      [-121.24390376618041, 44.73079940283668],
      [-121.24978316834105, 44.73079940283668],
      [-121.24978316834105, 44.7240001852607]]]
);

var oakSpringsAOI = ee.Geometry.Polygon(
    // Oak Springs AOI Coordinates:
    [[[-121.0828856334261, 45.21996624074323],
      [-121.07730663867513, 45.21996624074323],
      [-121.07730663867513, 45.22676736033],
      [-121.0828856334261, 45.22676736033],
      [-121.0828856334261, 45.21996624074323]]]
);

// =====================================================================
// STEP 2: SELECT THE ACTIVE AOI FOR ANALYSIS (EASY SWAP)

// To switch regions, uncomment the line you want to use.
// -------------------------------------------------------------
var ACTIVE_AOI = peltonTrapAOI; // <-- ANALYSIS WILL RUN FOR PELTON TRAP
// var ACTIVE_AOI = oakSpringsAOI; // <-- ANALYSIS WILL RUN FOR OAK SPRINGS
// -------------------------------------------------------------

// Use a variable name that will be used globally for the analysis
var lowerDeschutesAOI = ACTIVE_AOI; 


// =====================================================================
// STEP 3: CORE FUNCTIONS AND DATA LOADING

// 3a. Define the Robust Multi-Band NDCI Calculation and Water Mask Function
var calculateNDCI = function(image) {
  // Select the necessary bands
  var B2 = image.select('B2'); 
  var B3 = image.select('B3'); 
  var B4 = image.select('B4'); 
  var B5 = image.select('B5'); 
  var B8 = image.select('B8'); 

  // Calculate NDCI: (B5 - B4) / (B5 + B4)
  var ndci = B5.subtract(B4).divide(B5.add(B4)).rename('NDCI');
  
  // --- REFINED ROBUST MULTI-BAND WATER MASK ---
  var ndwi = B3.subtract(B8).divide(B3.add(B8));
  var ndwiMask = ndwi.gte(0.05); 
  var nirMask = B8.lt(1200); 
  var blueMask = B2.lt(1800); 
  var waterMask = ndwiMask.and(nirMask).and(blueMask);

  // Mask the NDCI image
  var masked_ndci = ndci.updateMask(waterMask);

  // Return the original image with the new, masked NDCI band added
  return image.addBands(masked_ndci);
};

// 3b. Load and Filter the Sentinel-2 Collection using the ACTIVE_AOI
var s2_collection = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
    // Set your desired date range here (e.g., a specific summer since 2017)
    .filterDate('2019-01-10', '2020-09-30') 
    .filterBounds(lowerDeschutesAOI)
    .filterMetadata('CLOUD_COVERAGE_ASSESSMENT', 'less_than', 10); 

// 4. Apply the function to the Image Collection
var ndci_collection = s2_collection.map(calculateNDCI); 


// =====================================================================
// STEP 4: CREATE COMPOSITES AND VISUALIZATION

// 5a. Create the NDCI Composite
var ndci_composite = ndci_collection.select('NDCI').median(); 

// 5b. Create a separate True Color Composite
var true_color_composite = ndci_collection.select('B4', 'B3', 'B2').median();


// 6. Define Visualization Parameters
var ndci_vis = {
  min: -0.06,  
  max: 0.30,   
  palette: [
    '0000FF', // Deep Blue (Low Chl-a)
    '00FF00', // Green
    'FF0000'  // Red (High Chl-a)
  ]
};

var trueColorVis = {
  min: 0, 
  max: 2000, 
  bands: ['B4', 'B3', 'B2'] 
}; 


// =====================================================================
// STEP 5: MAP DISPLAY AND TIME SERIES PLOT

// 7. Display the Result on the Map
Map.centerObject(lowerDeschutesAOI, 12); 

// Layer 1: The NDCI/Chlorophyll Map
Map.addLayer(ndci_composite, ndci_vis, 'NDCI (Chlorophyll) for Active AOI');

// Layer 2: The True Color Context Map
Map.addLayer(true_color_composite, trueColorVis, 'True Color Context', false);

// 8. Print the Time Series Plot
var chart = ui.Chart.image.series({
  imageCollection: ndci_collection.select('NDCI'), 
  region: lowerDeschutesAOI, 
  reducer: ee.Reducer.median(), 
  scale: 10, 
  xProperty: 'system:time_start' 
})
.setOptions({
  title: 'Median NDCI Time Series for Active Region',
  vAxis: {
      title: 'Median NDCI',
      minValue: -0.06, 
      maxValue: 0.30   
  }, 
  hAxis: {
      title: 'Date', 
      format: 'MMM yyyy' // Clearer format showing the month and full year
  },
  // ----------------------------------------
  lineWidth: 2,
  colors: ['#0077B6'] 
});
print(chart);