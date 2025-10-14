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

// --- STRIDE Loading Screen Control ---
// --- STRIDE Loading Screen Control (Only after login) ---
$(document).ready(function () {

  // Function to show loader only in dashboard mode
  function showLoader(text) {
    if ($("body").hasClass("dashboard-bg")) {
      if (text) $("#loading-text").text(text);
      $("#loading-overlay").fadeIn(150);
    }
  }

  function hideLoader() {
    if ($("body").hasClass("dashboard-bg")) {
      setTimeout(function () {
        $("#loading-overlay").fadeOut(500);
      }, 500);
    }
  }

  // --- STRIDE Loading Screen Control (Only after login) ---
$(document).on("shiny:connected", function() {
  // Only show loader if user is already in dashboard mode
  if ($("body").hasClass("dashboard-bg")) {
    $("#loading-overlay").fadeIn(300);
  } else {
    $("#loading-overlay").hide(); // Hide loader on login page
  }
});

$(document).on("shiny:value", function() {
  // Fade out loader only when in dashboard
  if ($("body").hasClass("dashboard-bg")) {
    setTimeout(function() {
      $("#loading-overlay").fadeOut(800);
      $("body").addClass("loaded");
    }, 600);
  }
});


});

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

// === SAFE SMOOTH DROP-IN FOR LEAFLET MARKERS ===
// === RELIABLE LEAFLET MARKER DROP ANIMATION ===
$(document).on("shiny:connected", function() {
  // Wait for map container to exist
  const waitForMap = setInterval(() => {
    const mapPane = document.querySelector("#TextMapping .leaflet-marker-pane");
    if (mapPane) {
      clearInterval(waitForMap);

      // Watch for new marker icons being added inside the map
      const observer = new MutationObserver((mutationsList) => {
        mutationsList.forEach(mutation => {
          mutation.addedNodes.forEach(node => {
            if (node.classList && node.classList.contains("leaflet-marker-icon")) {
              // Animate each new marker
              const markers = document.querySelectorAll("#TextMapping .leaflet-marker-icon");
              markers.forEach((marker, index) => {
                marker.style.opacity = "0";
                marker.style.transform = "translateY(-10px) scale(0.9)";
                marker.style.animation = "dropMarker 0.6s ease-out forwards";
                marker.style.animationDelay = `${index * 0.1}s`; // delay between markers
              });
            }
          });
        });
      });

      observer.observe(mapPane, { childList: true });
    }
  }, 300);
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






