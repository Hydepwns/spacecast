const FileDownload = {
  mounted() {
    this.handleEvent("download_file", ({ content, filename, mime_type }) => {
      this.downloadFile(content, filename, mime_type);
    });
  },

  downloadFile(content, filename, mime_type) {
    // Create a blob from the content
    const blob = new Blob([content], { type: mime_type });
    
    // Create a URL for the blob
    const url = URL.createObjectURL(blob);
    
    // Create a temporary link element
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    
    // Append to body, click, and remove
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    
    // Clean up the URL
    URL.revokeObjectURL(url);
  }
};

export default FileDownload; 