function animateCards() {
  const cards = document.querySelectorAll(".card");
  cards.forEach(card => {
    if (!card.classList.contains("animate")) {
      card.classList.add("hidden");
      // Trigger reflow so transition applies
      void card.offsetWidth;
      card.classList.add("animate");
      card.classList.remove("hidden");
    }
  });
}

document.addEventListener("DOMContentLoaded", animateCards);

// Re-run animation when Shiny updates the UI
$(document).on('shiny:value', function() {
  animateCards();
});

// Sync card + text scaling with page zoom
function syncDashboardScale() {
  const scale = window.innerWidth / window.outerWidth;
  document.documentElement.style.setProperty('--zoom-scale', scale);
}

window.addEventListener('resize', syncDashboardScale);
window.addEventListener('load', syncDashboardScale);


// Auto-show dropdowns on hover (desktop only)
$(document).ready(function() {
  if (window.innerWidth > 992) { // desktop only
    $(".navbar .dropdown").hover(
      function() {
        $(this).addClass("show");
        $(this).find(".dropdown-menu").addClass("show");
      },
      function() {
        $(this).removeClass("show");
        $(this).find(".dropdown-menu").removeClass("show");
      }
    );
  }
});


// === REPLAY SIDEBAR ANIMATION ON NAV SWITCH ===
$(document).on("click", ".nav-link", function() {
  const sidebar = document.querySelector(".bslib-sidebar-layout > .sidebar");
  if (sidebar) {
    sidebar.style.animation = "none";
    sidebar.offsetHeight; // trigger reflow
    sidebar.style.animation = "sidebarSlideIn 0.6s ease-out";
  }
});


// === REPLAY BODY + SIDEBAR ANIMATIONS ON NAV SWITCH ===
$(document).on("click", ".nav-link", function() {
  const sidebar = document.querySelector(".bslib-sidebar-layout > .sidebar");
  const main = document.querySelector(".bslib-sidebar-layout > .main");

  [sidebar, main].forEach(el => {
    if (el) {
      el.style.animation = "none";
      el.offsetHeight; // reflow
      const animName = el.classList.contains("sidebar") ? "sidebarSlideIn" : "bodyFadeIn";
      el.style.animation = `${animName} 2s ease-out`;
    }
  });
});

// === LOADER CONTROL ===
function showLoader(text) {
  if (text) $("#loading-text").text(text);
  $("#loading-overlay")
    .stop(true, true)
    .fadeIn(600)
    .css("display", "flex");
}

function hideLoader() {
  $("#loading-overlay").stop(true, true).fadeOut(1200);
}

// === SHINY MESSAGE HANDLERS ===
Shiny.addCustomMessageHandler("showLoader", function(message) {
  showLoader(message);
});

Shiny.addCustomMessageHandler("hideLoader", function(message) {
  hideLoader();
});

Shiny.addCustomMessageHandler("addDashboardClass", function(message) {
  $("body").addClass("dashboard-bg");
});

// === Smart Loader Control ===

// Flag to track rendering progress
let shinyBusy = true;

// When Shiny starts recalculating
$(document).on("shiny:busy", function() {
  shinyBusy = true;
  console.log("🚧 shiny busy — still loading");
});

// When Shiny becomes idle (initial render done)
$(document).on("shiny:idle", function() {
  console.log("✅ shiny idle — checking if visuals are done...");

  // Wait a bit to make sure UI elements (plots, tables, etc.) are visible
  setTimeout(() => {
    if (!shinyBusy) return; // Prevent duplicate hides
    shinyBusy = false;

    // Double-check DOM readiness
    if ($(".plotly, .datatables, .leaflet-container, .bslib-card").length > 0) {
      console.log("✨ All visuals appear loaded, hiding loader.");
      hideLoader();
    } else {
      // Try again in 1 second if visuals not yet visible
      console.log("⏳ Waiting for UI elements...");
      setTimeout(() => hideLoader(), 1000);
    }
  }, 1500); // wait a little after idle event
});

// Hide loader only after everything’s ready
$(window).on("load", function() {
  setTimeout(() => hideLoader(), 2000); // fallback if shiny events fail
});

// Ensure overlay hidden when page loads fresh
$(window).on("load", function() {
  $("#loading-overlay").hide();
});



Shiny.addCustomMessageHandler("setLoginMode", function(mode) {
  if (mode === "login") {
    document.body.classList.add("login-hidden");
  } else {
    document.body.classList.remove("login-hidden");
  }
});

// === INTERACTIVE BUBBLES ===
document.addEventListener("DOMContentLoaded", () => {
  const bubbles = document.querySelectorAll(".bubble");

  bubbles.forEach((bubble) => {
    // Random animation delay so they don't move in sync
    bubble.style.animationDelay = `${Math.random() * 8}s`;

    // Random slow float speed
    bubble.style.animationDuration = `${10 + Math.random() * 10}s`;

    // Hover effect: tilt slightly toward cursor
    bubble.addEventListener("mousemove", (e) => {
      const rect = bubble.getBoundingClientRect();
      const x = e.clientX - rect.left - rect.width / 2;
      const y = e.clientY - rect.top - rect.height / 2;
      bubble.style.transform = `translate(${x / 10}px, ${y / 10}px) scale(1.1)`;
    });

    bubble.addEventListener("mouseleave", () => {
      bubble.style.transform = "translate(0, 0) scale(1)";
    });

    // Click pulse effect1
    bubble.addEventListener("click", () => {
      bubble.style.animation = "pulsePop 0.6s ease";
      setTimeout(() => {
        bubble.style.animation = `gentleFloat ${10 + Math.random() * 10}s ease-in-out infinite`;
      }, 600);
    });
  });
});

// --- Enlarge register panel for HR/Engineer ---
$(document).on("change", "select[id$='govlev']", function() {
  const selected = $(this).val();
  const $body = $("body");
  
  if (selected === "HR" || selected === "Engineer") {
    $body.addClass("enlarge-panel");
  } else {
    $body.removeClass("enlarge-panel");
  }
});






