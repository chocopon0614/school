(function() {

var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  return $(scope).find(".nvd3-linechart2");
};

binding.renderValue = function(el, data) {

  var $el = $(el);

  if (!$el.data("state")) {
    //draw the line chart
        var chart = nv.models.lineChart()
            .margin({top: 30, right: 90, bottom: 50, left: 120})
            .x(function(d,i) { return i })
            .transitionDuration(350)
            .useInteractiveGuideline(true)
            .color(d3.scale.category10().range())
            ;

    chart.xAxis.tickFormat(function(d) {
        var dx = data[0].values[d] && data[0].values[d].x || 0;
        return d3.time.format('%x')(new Date(dx));
      });
      
    chart.yAxis  
      .axisLabel('Price')
      .tickFormat(d3.format(',f'));


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
Shiny.outputBindings.register(binding, "shinyjsexamples.nvd3-linechart2");

})();
