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
      el.style.animation = `${animName} 0.6s ease-out`;
    }
  });
});

// === STRIDE UNIVERSAL LOADING CONTROL ===
function showLoader(text) {
  if (text) $("#loading-text").text(text);
  $("#loading-overlay").stop(true, true).fadeIn(200);
}

function hideLoader() {
  $("#loading-overlay").stop(true, true).fadeOut(400);
}

// --- Register message handlers once ---
Shiny.addCustomMessageHandler("showLoader", function(message) {
  showLoader(message);
});

Shiny.addCustomMessageHandler("hideLoader", function(message) {
  hideLoader();
});

Shiny.addCustomMessageHandler("addDashboardClass", function(message) {
  $("body").addClass("dashboard-bg");
});

// --- Auto-hide loader when Shiny finishes rendering ---
$(document).on("shiny:connected", function() {
  setTimeout(() => hideLoader(), 2500);
});


// --- Ensure overlay hidden on page load ---
$(window).on("load", function() {
  $("#loading-overlay").hide();
});
$(document).on("shiny:idle", function() {
  console.log("✅ shiny:idle event fired");
});

$(document).on("shiny:idle", function() {
  console.log("✅ shiny:idle detected");
  $("#loading-overlay").fadeOut(400);
});





