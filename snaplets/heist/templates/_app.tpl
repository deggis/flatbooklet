
<div class="navbar navbar-inverse navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">
      <button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <a class="brand" href="#">Flatbooklet</a>
      <div class="nav-collapse collapse">
        <ul class="nav">
          <li class="active"><a href="#dashboard">Dashboard</a></li>
          <li><a href="#new">New</a></li>
          <li><a href="#statistics">Statistics</a></li>
          <li><a href="#logout">Logout</a></li>
        </ul>
      </div><!--/.nav-collapse -->
    </div>
  </div>
</div>

<div id="content" class="container">


  <!-- dummy elements to test JS functionality -->
  <div id="topic">
    <h2>Flatbooklet</h2>
  </div>
  <div id="statistics-container">
    <h3>Statistics</h3>
    <div id="statistics">
    </div>
  </div>
  <div id="viewer">
    <h3>View/update</h3>
    <fieldset>
      <legend>Viewer</legend>
      <label for="viewer-textarea">(add new)</label>
      <textarea rows="6" cols="60" id="viewer-textarea"></textarea>
      <button id="viewer-send" disabled>Add</button>
    </fieldset>
  </div>
  <div id="search">
    <h3>Search</h3>
    <fieldset>
      <legend>Search</legend>
      <input type="text" size="30" />
      <button id="search-button">Search</button>
    </fieldset>
  </div>
  <div id="latest-container">
    <h3>Latest</h3>
    <div id="latest">
    </div>
  </div>
  <div id="timeline-container">
    <h3>Timeline</h3>
    <div id="timeline">
    </div>
  </div>
  
  <div id="footer">
    <p><a href="/logout">Logout</a></p>
  </div>
</div>