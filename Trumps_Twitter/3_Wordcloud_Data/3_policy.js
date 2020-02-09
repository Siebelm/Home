
// loading data
var All3 = [{"word":"Crime","size":"1"},{"word":"Strong","size":"0.826086956521739"},{"word":"Hard","size":"0.652173913043478"},{"word":"Bad","size":"0.489130434782609"},{"word":"Illegal","size":"0.456521739130435"},{"word":"Fake","size":"0.41304347826087"},{"word":"Endorsement","size":"0.358695652173913"},{"word":"Win","size":"0.271739130434783"},{"word":"Crooked","size":"0.184782608695652"},{"word":"Honor","size":"0.141304347826087"},{"word":"Reform","size":"0.119565217391304"},{"word":"Weak","size":"0.0978260869565217"},{"word":"Tough","size":"0.0869565217391304"},{"word":"Fantastic","size":"0.0760869565217391"},{"word":"Congratulations","size":"0.0543478260869565"},{"word":"Fair","size":"0.0543478260869565"},{"word":"Protect","size":"0.0543478260869565"},{"word":"Wonderful","size":"0.0326086956521739"},{"word":"Incredible","size":"0.0108695652173913"},{"word":"Loves","size":"0"}];

var Econ3 = [{"word":"Strong","size":"1"},{"word":"Crime","size":"0.962962962962963"},{"word":"Hard","size":"0.851851851851852"},{"word":"Bad","size":"0.62962962962963"},{"word":"Endorsement","size":"0.555555555555556"},{"word":"Reform","size":"0.444444444444444"},{"word":"Win","size":"0.425925925925926"},{"word":"Fake","size":"0.388888888888889"},{"word":"Fantastic","size":"0.351851851851852"},{"word":"Crooked","size":"0.259259259259259"},{"word":"Fair","size":"0.222222222222222"},{"word":"Disaster","size":"0.166666666666667"},{"word":"Incredible","size":"0.166666666666667"},{"word":"Congratulations","size":"0.148148148148148"},{"word":"Tough","size":"0.12962962962963"},{"word":"Loves","size":"0.0925925925925926"},{"word":"Hit","size":"0.0555555555555556"},{"word":"Weak","size":"0.037037037037037"},{"word":"Protect","size":"0.0185185185185185"},{"word":"Worst","size":"0"}];

var Global3 = [{"word":"Honor","size":"1"},{"word":"Fake","size":"0.6"},{"word":"Wonderful","size":"0.6"},{"word":"Hard","size":"0.56"},{"word":"Attack","size":"0.48"},{"word":"Bad","size":"0.44"},{"word":"Strong","size":"0.32"},{"word":"Fair","size":"0.24"},{"word":"Peace","size":"0.16"},{"word":"Nice","size":"0.12"},{"word":"Progress","size":"0.12"},{"word":"Congratulations","size":"0.08"},{"word":"Win","size":"0.08"},{"word":"Crime","size":"0.04"},{"word":"Tough","size":"0.04"},{"word":"Weak","size":"0.04"},{"word":"Advantage","size":"0"},{"word":"Beautiful","size":"0"},{"word":"Love","size":"0"},{"word":"Success","size":"0"}];

var Immigration3 = [{"word":"Crime","size":"1"},{"word":"Illegal","size":"0.921875"},{"word":"Strong","size":"0.640625"},{"word":"Endorsement","size":"0.328125"},{"word":"Weak","size":"0.28125"},{"word":"Gang","size":"0.21875"},{"word":"Hard","size":"0.203125"},{"word":"Criminal","size":"0.15625"},{"word":"Bad","size":"0.140625"},{"word":"Loves","size":"0.109375"},{"word":"Protect","size":"0.109375"},{"word":"Dangerous","size":"0.09375"},{"word":"Fake","size":"0.078125"},{"word":"Tough","size":"0.078125"},{"word":"Safe","size":"0.0625"},{"word":"Win","size":"0.0625"},{"word":"Crooked","size":"0.03125"},{"word":"Desperately","size":"0.015625"},{"word":"Merit","size":"0.015625"},{"word":"Ridiculous","size":"0"}]
;


// setting the dimensions and margins of the chart
var margin = {top: 20, right: 0, bottom: 0, left: 0},
    width3 = 580 - margin.left - margin.right,
    height3 = 550 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg3 = d3.select("#_3_policy").append("svg")
    .attr("width", width3 + margin.left + margin.right)
    .attr("height", height3 + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// adding title
var Title = svg3.append("text")
		.attr("y", -5)
		.attr("font-size", "1.75em")
		.attr("text-anchor", "right")
		.text("Top Words Used");	


// updating data
function update3(data) {		

// creating word cloud
var layout = d3.layout.cloud()
  .size([width3, height3])
  .words(data.map(function(d) { return {text: d.word, size:d.size}; }))
  .padding(5)        //space between words
  .rotate(function() { var angle = ~~(Math.random() * 4)  * 45
							if (angle==135) {
								var flip = -45; 
								return flip;
							}
							else {
								return angle; 	
							}
						})
  .fontSize(function(d) { return d.size*90+30; })
  .on("end", draw);
layout.start();

// drawing top 20 words
function draw(words) {
	$("#remove3").remove();
	$("#remove3").remove();
	$("#remove3").remove();
	$("#remove3").remove();

	svg3
    .append("g")
	.attr("id", "remove3")
		.attr("transform", "translate(" + layout.size()[0] / 2 + "," + layout.size()[1] / 2 + ")")
		.selectAll("text")
			.data(words)
		.enter().append("text")
		.style("font-size", function(d) { return d.size; })
		.style("fill", "#A57706")
		.attr("text-anchor", "middle")
		.style("font-family", "Impact")
		.attr("transform", function(d) {
		return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
		})
		.text(function(d) { return d.text; })
		.on("mouseover", function(d){
			tempColor = this.style.fill;
			d3.select(this)
				.transition() 
				.style('fill', 'rgb(220, 217, 205)')
				.transition().delay(750).duration(5000)
				.style('fill', tempColor);;
			})
		.on("mouseout", function(d){
				d3.select(this)
					.transition().duration(500) 
					.style('fill', tempColor)
			})		
	}
}
// Initialize the plot with the first dataset
update3(All3);	