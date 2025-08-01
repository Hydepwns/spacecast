/**
 * Battery optimization utilities
 * Provides functions to detect battery status and implement optimizations
 * for low battery situations.
 */

/**
 * Checks if the device battery is in a low state (below 20%)
 * @returns {Promise<boolean>} Promise resolving to true if battery is low
 */
export async function isBatteryLow() {
  try {
    const batteryInfo = await getBatteryInfo();
    // Consider battery low if it's below 20% and not charging
    return batteryInfo && batteryInfo.level < 0.2 && !batteryInfo.charging;
  } catch (error) {
    console.warn("Unable to determine battery status:", error);
    return false; // Default to false if we can't determine battery status
  }
}

/**
 * Retrieves battery information using the Battery Status API
 * @returns {Promise<BatteryManager|null>} Battery information or null if not supported
 */
export async function getBatteryInfo() {
  // Check if Battery API is supported
  if (!navigator.getBattery) {
    console.info("Battery Status API not supported in this browser");
    return null;
  }

  try {
    const battery = await navigator.getBattery();
    return {
      level: battery.level, // Battery level between 0 and 1
      charging: battery.charging, // Boolean, true if device is charging
      chargingTime: battery.chargingTime, // Seconds until fully charged (Infinity if not charging)
      dischargingTime: battery.dischargingTime // Seconds until empty (Infinity if charging)
    };
  } catch (error) {
    console.error("Error accessing battery information:", error);
    return null;
  }
}

/**
 * Applies performance optimizations based on battery status
 * @param {Object} options - Configuration options
 * @param {boolean} options.reduceAnimations - Whether to reduce animations
 * @param {boolean} options.reduceFetchFrequency - Whether to reduce data fetching frequency
 * @returns {Promise<boolean>} Whether optimizations were applied
 */
export async function applyBatteryOptimizations(options = {
  reduceAnimations: true,
  reduceFetchFrequency: true
}) {
  const lowBattery = await isBatteryLow();
  
  if (lowBattery) {
    // Add low-battery class to the document for CSS optimizations
    document.documentElement.classList.add('low-battery-mode');
    
    console.info("Battery is low, applying performance optimizations");
    return true;
  } else {
    // Remove low-battery class if it exists
    document.documentElement.classList.remove('low-battery-mode');
    return false;
  }
}

/**
 * Sets up battery status monitoring
 * @param {Function} callback - Function to call when battery status changes
 * @returns {Function} Function to stop monitoring
 */
export function monitorBatteryStatus(callback) {
  if (!navigator.getBattery) {
    console.info("Battery status monitoring not available");
    return () => {}; // Return no-op cleanup function
  }

  let batteryObj = null;

  const handleBatteryChange = () => {
    const isLow = batteryObj && batteryObj.level < 0.2 && !batteryObj.charging;
    callback({
      level: batteryObj ? batteryObj.level : null,
      charging: batteryObj ? batteryObj.charging : null,
      isLow
    });
  };

  // Set up event listeners
  navigator.getBattery().then(battery => {
    batteryObj = battery;
    
    battery.addEventListener('levelchange', handleBatteryChange);
    battery.addEventListener('chargingchange', handleBatteryChange);
    
    // Initial call
    handleBatteryChange();
  });

  // Return cleanup function
  return () => {
    if (batteryObj) {
      batteryObj.removeEventListener('levelchange', handleBatteryChange);
      batteryObj.removeEventListener('chargingchange', handleBatteryChange);
    }
  };
} 