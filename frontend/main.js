const host = window.location.protocol + "//" + window.location.host;
const token = localStorage.getItem("token");
const currentThreadId = window.location.pathname.split("/")[2];

Vue.use(window['vueMoment']);

let threadsApp = new Vue({
  el: '#threads',
  data: {
    threads: [],
    newThreadTitle: "",
  },
  methods: {
    submitThread: event => {
      fetch(host + "/threads", {
        method: 'POST',
        body: JSON.stringify({name: threadsApp.newThreadTitle}),
        headers: {
          "Access-Control-Allow-Origin": "*",
          "Authorization": token
        }
      })
      .then(response => response.json())
      .then(data => {
        window.location.href = "/threads/" + data;
      });
    },
  }
});

let app = new Vue({
  el: '#app',
  data: {
    threadTitle: "",
    messages: [],
    authorName: "",
    content: "",
    active: false,
  },
  methods: {
    saveAuthor: function(event) {
      Cookies.set("username", this.authorName);
    },
    submitComment: function(event) {
      const data = {
        name: this.authorName,
        content: app.content
      };
      fetch(host + "/threads/" + currentThreadId + "/comments", {
          method: 'POST',
          body: JSON.stringify(data),
          headers: {
            "Access-Control-Allow-Origin": "*",
            "Authorization": token
          }
        })
        .then(response => response.json())
        .then(data => {
          this.content = "";
          console.log(data);
        });
    },
  }
});

let saved_name = Cookies.get("username");
if (saved_name !== undefined) {
  app.authorName = saved_name;
}

function reloadThreads(query) {
  fetch(host + "/threads", {
    method: "QUERY",
    headers: {
      "Authorization": token,
      "X-Comet-Wait": query.wait.toString()
    },
    body: JSON.stringify(query)
  })
  .then(response => {
    if (response.status == 403) {
      window.location.href = "/login?return_to=" + window.location.pathname;
    } else {
      response.json().then(data => {
        threadsApp.threads = data;
        for (item of data){
          if (item.ident == currentThreadId){
            app.threadTitle = item.name;
          }
        }
        reloadThreads({wait: true});
      });
    }
  });
}

function reloadComments(query) {
  fetch(host + "/threads/" + currentThreadId + "/comments", {
      method: "QUERY",
      headers: {
        "Authorization": token,
        "X-Comet-Wait": query.wait.toString()
      },
      body: JSON.stringify(query)
    })
    .then(response => {
      if (response.status == 403) {
        window.location.href = "/login?return_to=" + window.location.pathname;
      } else {
        response.json().then(data => {
          app.messages.push(...data);
          app.active = true;
          let next = query.since;
          if (data.length >= 1) {
            next = data[data.length - 1].date;
          }
          reloadComments({wait: true, since: next});
        });
      }
    });
}
reloadThreads({wait: false});

if (currentThreadId){
  reloadComments({wait: false, since: null});
}