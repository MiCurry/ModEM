#include "umpire/ResourceManager.hpp"
#include "umpire/strategy/QuickPool.hpp"
#include "umpire/strategy/DynamicPoolList.hpp"

extern "C" void setup_umpire_pool(){

    auto& rm = umpire::ResourceManager::getInstance();
    auto allocator = rm.getAllocator("HOST");
    //auto heuristic_function = umpire::strategy::QuickPool::blocks_releasable(2);
    //auto heuristic_function = umpire::strategy::DynamicPoolList::blocks_releasable(1);
    auto heuristic_function = umpire::strategy::DynamicPoolList::percent_releasable(75);


    auto pool = rm.makeAllocator<umpire::strategy::DynamicPoolList>(
            "HOST_POOL",
            allocator,
            1024ul * 1024ul * 35000ul, // Initial block size (2  MB)
            //1024ul * 1024ul * .1, // Minimum allocation size(1MB)
            1000000ul,
            16,
            heuristic_function
        );

}
