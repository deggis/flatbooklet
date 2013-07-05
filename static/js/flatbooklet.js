


function get_url_base () {
    return '/fb/'+flatbooklet.user;
}


var page_dashboard = function () {
    $('#content').html('<h2>Welcome</h2>');
}



function build_routes () {

    var routes = {};
    routes['#dashboard'] = page_dashboard; 
    
    var open_page = function (hash) {
        $('.nav li.active').removeClass('active');
        var mainmenu_item = $('a[href='+hash+']').closest('.mainmenu');
        $(mainmenu_item).addClass('active');
        
        var page = routes[hash] || function () { alert('Page '+hash+' not implemented'); }
        page();
    }
    
    $('.nav a:not(.dropdown-toggle)').click(function () {
        open_page($(this).attr('href'));
    });

    open_page('#dashboard');
}


$(document).ready(function () {
    build_routes();
});
