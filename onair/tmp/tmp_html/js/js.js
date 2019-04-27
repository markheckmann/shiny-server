
// toggle the info text boxes

function toggleAndChangeText(id) {
     id_div = '#' + id;
     id_toggle = '#' + "toggle-" + id;
     $(id_div).toggle();
     if ($(id_div).css('display') == 'none') {
          $(id_toggle).html('More info &#9660');
     } else {
          $(id_toggle).html('More info &#9650');
     }
}
