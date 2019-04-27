// tooltips for all buttons etc.
var delay =  {show: 100, hide: 100} ;
var tooltips = new Array();
var tooltips_permanent = new Array();
var tooltips_left = new Array();

// tooltips toggler
tooltips_permanent["#chk_toggle_tooltip_wrapper_div"] = "Activate this checkbox to get an info box like this on for every control element";
tooltips_left[".toggle-info"] = "Toggle info";

// Load grid

tooltips["#upload_dialog"] = "Upload a grid from your computer";
tooltips["#samplegrid"] = "Select a grid to get started and explore the app right away, without uploading a grid";

// Bertin

tooltips["#bertin_settings"] = "In this panel you can adjust the settings for the Bertin display";
tooltips["#bertin_standard_showvalues_wrapper_div"] = "Whether to show the grid ratings";
tooltips["#bertin_standard_cex_all"] = "Text size";
tooltips["#bertin_standard_xlim_1"] = "Width of left construct margin";
tooltips["#bertin_standard_xlim_2"] = "Width of right construct margin";
tooltips["#bertin_standard_ylim"] = "Height of elements region";
tooltips["#bertin_standard_color_left"] = "Color for left pole";
tooltips["#bertin_standard_color_right"] = "Color for right pole";



// add tooltips to all buttons etc.
//
function initiate_tooltips(array, placement) 
{
  placement = typeof placement !== 'undefined' ? placement : "right";    // right as default placement
  for (var key in array) {
     $( key ).tooltip({html: true, 
                             title:  array[key], 
                             trigger: "hover",
                             placement: placement,
                             delay: delay
                            }); 
  } 
}


function disable_tooltips()
{
  for (var key in tooltips) 
     $( key ).tooltip('disable');
}


function enable_tooltips()
{
  for (var key in tooltips) 
     $( key ).tooltip('enable');
}


function toggle_tooltips() {
  if (this.checked)
    enable_tooltips();
  else
    disable_tooltips();
}

// after document is loaded init all tooltips 
// and disable themn right away
//
$(document).ready(function() {
  initiate_tooltips(tooltips);
  initiate_tooltips(tooltips_left);
  initiate_tooltips(tooltips_permanent);
  disable_tooltips();
  $("#chk_toggle_tooltips").change(toggle_tooltips);   // add event to toggleToolstip button
});  

