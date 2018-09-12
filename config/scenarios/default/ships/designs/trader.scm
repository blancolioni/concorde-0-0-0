(define-ship 'trader-1
   (component 'basic-hold 'main-hold)
   (component 'basic-chemical-drive 'drive-1 (attach 1 'main-hold 5))
)
