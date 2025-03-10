#pragma once
#include "../Allocator.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Keep a pointer to the heap allocation as a member                      
   /// Manage its ownership                                                   
   ///   @tparam ID - which heap are we keeping track of?                     
   ///                                                                        
   template<unsigned ID = 0>
   struct OwnershipStack {
   private:
      Allocation* mAllocation;

   public:
      using CTTI_Component = Yes;
   };
   
   ///                                                                        
   /// Keep a pointer to the heap allocation as a member                      
   /// Just for padding to keep binary-compatibility - doesn't reference      
   ///   @tparam ID - which heap are we keeping track of?                     
   ///                                                                        
   template<unsigned ID = 0>
   struct NoOwnershipStack {
   private:
      Allocation* mAllocation;

   public:
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
