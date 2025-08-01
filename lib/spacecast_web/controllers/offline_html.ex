defmodule SpacecastWeb.OfflineHTML do
  use SpacecastWeb, :html

  def render(%{} = assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en" class="offline-page">
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta name="color-scheme" content="light dark" />
        <title>Hydepwns - Offline</title>

        <style>
          :root {
            /* Typography */
            --font-family: 'Monaspace Argon', 'JetBrains Mono', monospace;
            --line-height: 1.20rem;
            --border-thickness: 2px;
            
            /* Colors - default light theme */
            --text-color: #000;
            --text-color-alt: #666;
            --background-color: #fff;
            --background-color-alt: #eee;
          }

          @media (prefers-color-scheme: dark) {
            :root {
              --text-color: #fff;
              --text-color-alt: #aaa;
              --background-color: #000;
              --background-color-alt: #111;
            }
          }

          html, body {
            font-family: var(--font-family);
            color: var(--text-color);
            background-color: var(--background-color);
            line-height: var(--line-height);
            padding: 0;
            margin: 0;
          }

          .offline-container {
            max-width: 80ch;
            margin: 0 auto;
            padding: calc(var(--line-height) * 2) 2ch;
            display: flex;
            flex-direction: column;
            align-items: center;
            text-align: center;
          }

          h1 {
            font-size: 2rem;
            margin-bottom: calc(var(--line-height) * 2);
          }

          .offline-ascii-art {
            white-space: pre;
            font-family: var(--font-family);
            margin: calc(var(--line-height) * 2) 0;
            text-align: left;
          }

          p {
            margin-bottom: var(--line-height);
          }

          .offline-status {
            display: flex;
            align-items: center;
            justify-content: center;
            gap: 1ch;
            margin: calc(var(--line-height) * 2) 0;
            padding: var(--line-height) 2ch;
            background-color: var(--background-color-alt);
            border: var(--border-thickness) solid var(--text-color);
          }

          .status-indicator {
            display: inline-block;
            width: 1ch;
            height: 1ch;
            border-radius: 50%;
            background-color: #f00;
          }

          button {
            border: var(--border-thickness) solid var(--text-color);
            padding: calc(var(--line-height) / 2) 2ch;
            margin: var(--line-height) 0;
            font-family: var(--font-family);
            background-color: var(--background-color);
            color: var(--text-color);
            cursor: pointer;
          }

          button:hover {
            background-color: var(--background-color-alt);
          }

          .cached-content {
            margin-top: calc(var(--line-height) * 2);
            padding: var(--line-height) 2ch;
            border: var(--border-thickness) dashed var(--text-color-alt);
            text-align: left;
            max-width: 60ch;
          }

          .cached-content h2 {
            margin-top: 0;
          }

          .cached-content ul {
            padding-left: 4ch;
          }
        </style>
      </head>

      <body>
        <div class="offline-container">
          <h1>You're Offline</h1>

          <div class="offline-ascii-art">
            ┌─────────────────────────────────────┐
            │            OFFLINE MODE             │
            ├─────────────────────────────────────┤
            │                                     │
            │    ⚠️  No internet connection  ⚠️    │
            │                                     │
            │     But don't worry, some pages     │
            │     and features are available      │
            │     from the cache.                 │
            │                                     │
            └─────────────────────────────────────┘
          </div>

          <div class="offline-status">
            <span class="status-indicator"></span>
            <span>You are currently offline</span>
          </div>

          <p>We'll automatically reconnect when your internet connection is restored.</p>

          <button id="reload-button">Try to reconnect</button>

          <div class="cached-content">
            <h2>Available Offline</h2>
            <p>These pages may be available from the cache:</p>
            <ul id="cached-pages">
              <li>Home page</li>
              <li>Style Guide</li>
              <li>Previously visited pages</li>
            </ul>
          </div>
        </div>

        <script>
          // Check network status on page load
          function updateNetworkStatus() {
            const statusIndicator = document.querySelector('.status-indicator');
            const statusText = document.querySelector('.offline-status span:last-child');
            
            if (navigator.onLine) {
              statusIndicator.style.backgroundColor = '#0f0';
              statusText.textContent = 'Your internet connection is restored';
              
              // Automatically reload after a brief delay
              setTimeout(() => {
                window.location.reload();
              }, 3000);
            } else {
              statusIndicator.style.backgroundColor = '#f00';
              statusText.textContent = 'You are currently offline';
            }
          }

          // Check network status periodically
          setInterval(updateNetworkStatus, 5000);

          // Handle reload button click
          document.getElementById('reload-button').addEventListener('click', () => {
            window.location.reload();
          });

          // Listen for online/offline events
          window.addEventListener('online', updateNetworkStatus);
          window.addEventListener('offline', updateNetworkStatus);

          // Initial status check
          updateNetworkStatus();
        </script>
      </body>
    </html>
    """
  end
end
