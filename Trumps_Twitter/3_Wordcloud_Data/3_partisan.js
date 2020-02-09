
// loading data
var All3 = [{"word":"Crooked","size":"1"},{"word":"Bad","size":"0.367588932806324"},{"word":"Win","size":"0.284584980237154"},{"word":"Fake","size":"0.209486166007905"},{"word":"Crime","size":"0.181818181818182"},{"word":"Witch","size":"0.130434782608696"},{"word":"Collusion","size":"0.126482213438735"},{"word":"Strong","size":"0.122529644268775"},{"word":"Illegal","size":"0.118577075098814"},{"word":"Hard","size":"0.0909090909090909"},{"word":"Angry","size":"0.0869565217391304"},{"word":"Weak","size":"0.075098814229249"},{"word":"Wow","size":"0.0355731225296443"},{"word":"Phony","size":"0.0316205533596838"},{"word":"Lost","size":"0.0158102766798419"},{"word":"Won","size":"0.0158102766798419"},{"word":"Congratulations","size":"0.0118577075098814"},{"word":"Dishonest","size":"0.0118577075098814"},{"word":"Endorsement","size":"0.00790513833992095"},{"word":"Failed","size":"0"}];

var Republican3 = [{"word":"Win","size":"1"},{"word":"Strong","size":"0.428571428571429"},{"word":"Fake","size":"0.357142857142857"},{"word":"Crime","size":"0.285714285714286"},{"word":"Bad","size":"0.261904761904762"},{"word":"Congratulations","size":"0.261904761904762"},{"word":"Endorsement","size":"0.261904761904762"},{"word":"Lyin","size":"0.166666666666667"},{"word":"Failed","size":"0.119047619047619"},{"word":"Hard","size":"0.119047619047619"},{"word":"Loves","size":"0.119047619047619"},{"word":"Wow","size":"0.119047619047619"},{"word":"Proud","size":"0.0952380952380952"},{"word":"Won","size":"0.0952380952380952"},{"word":"Lost","size":"0.0714285714285714"},{"word":"Tough","size":"0.0476190476190476"},{"word":"Lightweight","size":"0.0238095238095238"},{"word":"Supreme","size":"0.0238095238095238"},{"word":"Victory","size":"0.0238095238095238"},{"word":"Honor","size":"0"}];

var Democrat3 = [{"word":"Crooked","size":"1"},{"word":"Bad","size":"0.309433962264151"},{"word":"Fake","size":"0.158490566037736"},{"word":"Witch","size":"0.154716981132075"},{"word":"Collusion","size":"0.150943396226415"},{"word":"Crime","size":"0.143396226415094"},{"word":"Win","size":"0.143396226415094"},{"word":"Illegal","size":"0.113207547169811"},{"word":"Angry","size":"0.10188679245283"},{"word":"Hard","size":"0.0641509433962264"},{"word":"Weak","size":"0.060377358490566"},{"word":"Phony","size":"0.0490566037735849"},{"word":"Strong","size":"0.0490566037735849"},{"word":"Crazy","size":"0.00754716981132075"},{"word":"Dishonest","size":"0.00754716981132075"},{"word":"Hoax","size":"0.00377358490566038"},{"word":"Terrible","size":"0.00377358490566038"},{"word":"Won","size":"0.00377358490566038"},{"word":"Wow","size":"0.00377358490566038"},{"word":"Corrupt","size":"0"}];



// setting the dimensions and margins of the chart
var margin = {top: 20, right: 0, bottom: 0, left: 0},
    width3 = 580 - margin.left - margin.right,
    height3 = 550 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg3 = d3.select("#_3_partisan").append("svg")
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