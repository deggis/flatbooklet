


function get_url_base () {
    return '/fb/'+flatbooklet.user;
}


$(document).ready(function () {
    $.get(get_url_base()+'/stats', function (data) {
        $('#statistics').html(data);
    });
});
