/**
 * SymbolizeR Documentation Site - Interactive Layout JavaScript
 */

(function() {
  'use strict';

  // ===== Mobile Sidebar Toggle =====
  function initSidebar() {
    const sidebar = document.querySelector('.sidebar');
    const overlay = document.querySelector('.sidebar-overlay');
    const menuBtn = document.querySelector('.mobile-menu-btn');
    
    if (!sidebar || !menuBtn) return;
    
    menuBtn.addEventListener('click', () => {
      sidebar.classList.toggle('active');
      overlay?.classList.toggle('active');
      document.body.style.overflow = sidebar.classList.contains('active') ? 'hidden' : '';
    });
    
    overlay?.addEventListener('click', () => {
      sidebar.classList.remove('active');
      overlay.classList.remove('active');
      document.body.style.overflow = '';
    });
    
    // Close sidebar on escape key
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape' && sidebar.classList.contains('active')) {
        sidebar.classList.remove('active');
        overlay?.classList.remove('active');
        document.body.style.overflow = '';
      }
    });
  }

  // ===== Active Page Highlighting =====
  function highlightActivePage() {
    const currentPath = window.location.pathname.split('/').pop() || 'index.html';
    const sidebarLinks = document.querySelectorAll('.sidebar-nav a');
    
    sidebarLinks.forEach(link => {
      const linkPath = link.getAttribute('href');
      if (linkPath === currentPath || 
          (currentPath === '' && linkPath === 'index.html') ||
          (currentPath === 'index.html' && linkPath === 'index.html')) {
        link.classList.add('active');
      } else {
        link.classList.remove('active');
      }
    });
  }

  // ===== Table of Contents Generation =====
  function generateTOC() {
    const tocContainer = document.querySelector('.toc-list');
    if (!tocContainer) return;
    
    const headings = document.querySelectorAll('.main-content h2, .main-content h3');
    if (headings.length === 0) {
      // Hide TOC sidebar if no headings
      const tocSidebar = document.querySelector('.toc-sidebar');
      if (tocSidebar) tocSidebar.style.display = 'none';
      return;
    }
    
    tocContainer.innerHTML = '';
    
    headings.forEach((heading, index) => {
      // Ensure heading has an ID
      if (!heading.id) {
        heading.id = 'section-' + index;
      }
      
      const li = document.createElement('li');
      const a = document.createElement('a');
      a.href = '#' + heading.id;
      a.textContent = heading.textContent;
      a.dataset.target = heading.id;
      
      if (heading.tagName === 'H3') {
        a.classList.add('toc-h3');
      }
      
      li.appendChild(a);
      tocContainer.appendChild(li);
    });
  }

  // ===== Scroll Spy for TOC =====
  function initScrollSpy() {
    const tocLinks = document.querySelectorAll('.toc-list a');
    if (tocLinks.length === 0) return;
    
    const headingElements = [];
    tocLinks.forEach(link => {
      const target = document.getElementById(link.dataset.target);
      if (target) headingElements.push({ link, target });
    });
    
    function updateActiveLink() {
      const scrollPos = window.scrollY + 100;
      let activeLink = null;
      
      headingElements.forEach(({ link, target }) => {
        if (target.offsetTop <= scrollPos) {
          activeLink = link;
        }
      });
      
      tocLinks.forEach(link => link.classList.remove('active'));
      if (activeLink) activeLink.classList.add('active');
    }
    
    window.addEventListener('scroll', updateActiveLink, { passive: true });
    updateActiveLink();
  }

  // ===== Smooth Scroll for Anchor Links =====
  function initSmoothScroll() {
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
      anchor.addEventListener('click', function(e) {
        const targetId = this.getAttribute('href').slice(1);
        const target = document.getElementById(targetId);
        
        if (target) {
          e.preventDefault();
          target.scrollIntoView({ behavior: 'smooth' });
          history.pushState(null, null, '#' + targetId);
        }
      });
    });
  }

  // ===== Search Modal Placeholder =====
  function initSearchShortcut() {
    const searchBtn = document.querySelector('.header-search');
    if (!searchBtn) return;
    
    function showSearchHint() {
      alert('Search functionality coming soon! For now, use Ctrl+F to search the page.');
    }
    
    searchBtn.addEventListener('click', showSearchHint);
    
    document.addEventListener('keydown', (e) => {
      if ((e.ctrlKey || e.metaKey) && e.key === 'k') {
        e.preventDefault();
        showSearchHint();
      }
    });
  }

  // ===== Initialize Everything =====
  function init() {
    initSidebar();
    highlightActivePage();
    generateTOC();
    initScrollSpy();
    initSmoothScroll();
    initSearchShortcut();
  }

  // Run on DOMContentLoaded
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
