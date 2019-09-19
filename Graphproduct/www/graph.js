(function() {

var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  return $(scope).find(".nvd3-linechart");
};

binding.renderValue = function(el, data) {

  var $el = $(el);

  if (!$el.data("state")) {
        //draw the bar chart
        var chart = nv.models.discreteBarChart()
            .margin({top: 30, right: 50, bottom: 50, left: 420})
            .transitionDuration(350)
            .tooltips(false) 
            .showValues(true)
            .staggerLabels(true)
            .color(d3.scale.category10().range())
            ;


    chart.yAxis  
      .axisLabel('Price')
      .tickFormat(d3.format(',r'));


    nv.utils.windowResize(chart.update);
    
    var selection = d3.select(el).select("svg");
    
    $el.data("state", {
      chart: chart,
      selection: selection
    });
  }
  

  var state = $el.data("state");
  
  nv.addGraph(function() {
    state.selection
      .datum(data)
      .transition()
      .duration(0)
      .call(state.chart);
    return state.chart;
  });
};

//binding with Shiny ui
Shiny.outputBindings.register(binding, "shinyjsexamples.nvd3-linechart");

})();
