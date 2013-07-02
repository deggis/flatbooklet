<apply template="base">

  <ifLoggedIn>

    <!-- dummy elements to test JS functionality -->
  
    <div id="topic">
      <h2>Flatbooklet</h2>
    </div>
    <div id="viewer">
      <fieldset>
        <legend>Viewer</legend>
        <label for="viewer-textarea">(add new)</label>
        <textarea rows="6" cols="60" id="viewer-textarea"></textarea>
        <button id="viewer-send" disabled>Add</button>
      </fieldset>
    </div>
    <div id="search">
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

    <div id="footer">
      <p><a href="/logout">Logout</a></p>
    </div>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
