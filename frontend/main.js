const host = window.location.protocol + "//" + window.location.host;
const token = localStorage.getItem("token");
Vue.use(window['vueMoment']);

let app = new Vue({
  el: '#app',
  data: {
    messages: [],
    authorName: "",
    content: ""
  },
  methods: {
    saveAuthor: function(event) {
      Cookies.set("username", this.authorName);
    },
    submit: function(event) {
      const data = {
        name: this.authorName,
        content: app.content
      };
      fetch(host + "/comments", {
          method: 'POST',
          body: JSON.stringify(data),
          headers: {
            "Access-Control-Allow-Origin": "*",
            "Authorization": token
          }
        })
        .then(response => response.json())
        .then(data => console.log(data));
    }
  }
});

let saved_name = Cookies.get("username");
if (saved_name !== undefined) {
  app.authorName = saved_name;
}

function reload(query) {
  fetch(host + "/comments", {
      method: "QUERY",
      headers: {
        "Authorization": token
      },
      body: JSON.stringify(query)
    })
    .then(response => {
      if (response.status == 403) {
        window.location.href = "/login";
      } else {
        response.json().then(data => {
          app.messages.push(...data);
          let next = query.since;
          if (data.length >= 1) {
            next = data[data.length - 1][1].date;
          }
          reload({wait: true, since: next});
        });
      }
    });
}

reload({wait: false, since: null});